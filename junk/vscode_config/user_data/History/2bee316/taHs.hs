{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE NamedFieldPuns #-}
-- {-# LANGUAGE RecordWildCards, DeriveGeneric, NamedFieldPuns, LambdaCase, ScopedTypeVariables #-}
module FullNode where
import MessageHandlers (combineHandlers, MessageHandler, answerContactQuery, answerBlockchainQuery, MsgHandler (MsgHandler), answerPing, receiveTransaction, TransactionQueue (TransactionQueue, getTransactionQueue), toServerHandler, removeUsedTransactions)
import Hashing (TargetHash, shash256, difficultyToTargetHash)
import BlockType (Genesis, Transaction (Transaction), BlockReference, blockBlockHeight, Block (Block, blockHeader, coinbaseTransaction))
import BlockChain (FixedBlocks, Fixed(Fixed, getFixedBlocks), Lively (Lively, root, forest), LivelyBlocks, Future (Future), FutureBlocks, BlockchainUpdated (BlockInserted, BLockInsertedLinksToRoot, FutureBlock, BlockInvalid, BlockAlreadyInserted), getLastBlock, updateWithBlock, ForkMaxDiff, collectUTXOs, newfixed2UTXOPoolUpdate, drawLively)
import Control.Concurrent.STM (TVar, STM, atomically, readTVar, readTVarIO, retry, writeTVar, newTVarIO, newTMVarIO, newTVar, modifyTVar')
import BlockValidation (UTXOPool (UTXOPool), validTransaction, UTXO (UTXO))
import qualified Data.Sequence as Seq
import Node (PeersSet, RunningApp (RunningApp), broadcastAndUpdatePeers, makeLogger, catchUpToBlockchain, withLogging, insertPeer, Status (Active), AppendFixed (appendFixed), generateKeys, HasDB (executeDBEither), acquire, topLevelErrorLog)
import BlockCreation (SimpleWallet, blockRef, mineBlock, Keys (Keys), OwnedUTXO (OwnedUTXO))
import Data.Aeson (ToJSON, FromJSON, eitherDecodeFileStrict, eitherDecodeFileStrict', encodeFile)
import Network.Socket (ServiceName)
import GHC.Generics (Generic)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Control.Monad (forever, join, void, liftM2, (>=>), when)
import Data.Foldable (toList, foldl')
import Control.Concurrent.Async (race_, async, concurrently_, waitBoth)
import Control.Concurrent (threadDelay, newMVar, forkIO)
import Data.Time (getCurrentTime)
import qualified Crypto.Random.DRBG as DRBG
import qualified Codec.Crypto.RSA as RSA
import Crypto.Random (newGenIO)
import Control.DeepSeq (force)
import Control.Exception (evaluate, onException, bracket)
import MessageType (Message(BlockMessage), ReceivedBlock (ReceivedBlock), Answer (BlockAnswer), ReceivedTransaction (ReceivedTransaction))
-- import Control.Monad.Except (runExceptT, withExceptT, ExceptT (ExceptT))
import Server (Address(Address), server)
import InMemory (logger, HasLogging, InMemory (readMemory, writeMemory, modifyMemory, modifyMemoryIO), runAtomically, InMemoryRead (readMemoryIO))
import System.Exit (exitFailure)
import Configs (PoolSettings, LoggingMode)
import Hasql.Pool (Pool)
import qualified Hasql.Pool as Pool
import Hasql.Session (Session)
import Wallet.Session as Wallet (insertOwnedUTXO, insertTransaction)
import Text.Pretty.Simple (pShow)
import Data.Text.Lazy (unpack)
import Wallet.Type (StoredTransaction(StoredTransaction), Status (Waiting))


data Config = Config {
        blockchainFilepath :: FilePath,
        peersFilepath :: FilePath,
        targetDifficulty   :: Int,
        loggingMode :: LoggingMode,
        port        :: ServiceName,
        blockchainGenesis     :: Genesis,
        minerWaitForTxs :: Bool,
        forkMaxDiff :: ForkMaxDiff,
        walletDatabaseConfig :: Maybe PoolSettings
    } deriving (Generic)

instance ToJSON Config
instance FromJSON Config

data AppState = AppState {
    blockchainState :: BlockchainState,
    incomingTxs  :: TVar TransactionQueue,
    peers        :: TVar PeersSet,
    minerWallet  :: TVar SimpleWallet
    }

-- LivelyBlocks by definition contains at least one block, so we differentiate between the empty case.
-- The invariant is that root of LivelyBlocks is the next block after the head of FixedBlocks.
-- Also UTXOPool is a pool of txs up of all FixedBlocks.   
-- TODO: ^ Verify if thats true.

-- data BlockchainState = BlockchainState Genesis (TVar FixedBlocks) (TVar LivelyBlocks) (TVar FutureBlocks) (TVar UTXOPool)
data BlockchainState = BlockchainState {
    getGenesis :: Genesis,
    getFixed :: TVar FixedBlocks,
    getLively :: TVar LivelyBlocks,
    getFuture :: TVar FutureBlocks,
    getUTXOPool :: TVar UTXOPool
}

data BlockchainData = BlockchainData FixedBlocks LivelyBlocks FutureBlocks UTXOPool

readFixedBlocks :: BlockchainState -> STM FixedBlocks
readFixedBlocks (BlockchainState _ fixed _ _ _) = readTVar fixed

readLivelyBlocks :: BlockchainState -> STM LivelyBlocks
readLivelyBlocks (BlockchainState _ _ lively _ _) = readTVar lively

readFutureBlocks :: BlockchainState -> STM FutureBlocks
readFutureBlocks (BlockchainState _ _ _ future _) = readTVar future

readUTXOPool :: BlockchainState -> STM UTXOPool
readUTXOPool (BlockchainState _ _ _ _ utxoPool) = readTVar utxoPool

writeFixedBlocks :: BlockchainState -> FixedBlocks -> STM ()
writeFixedBlocks (BlockchainState _ fixed _ _ _) = writeTVar fixed

writeLivelyBlocks :: BlockchainState -> LivelyBlocks -> STM ()
writeLivelyBlocks (BlockchainState _ _ lively _ _) = writeTVar lively

writeFutureBlocks :: BlockchainState -> FutureBlocks -> STM ()
writeFutureBlocks (BlockchainState _ _ _ future _) = writeTVar future

writeUTXOPool :: BlockchainState -> UTXOPool -> STM ()
writeUTXOPool (BlockchainState _ _ _ _ utxoPool) = writeTVar utxoPool

blocksEqual :: Block -> Block -> Bool
blocksEqual b1 b2 = shash256 (blockHeader b1) == shash256 (blockHeader b2)


mining :: ForkMaxDiff       -- constant specyfying at what difference between the length of blockchain forks the shorter fork is safe to discard. equivalent to at the depth at which block is considered done and dusted
       -> TargetHash        -- Need hash ∈ [0, targetHash] 
       -> AppState
       -> Bool              -- Do we wait for transaction or produce coinbase-only blocks
       -> (String -> IO ()) -- Logging function
       -> (forall a . Maybe (Session a -> IO (Either Pool.UsageError a))) -- db handle
       -> IO ()
mining forkMaxDiff targetHash appState@AppState {blockchainState, incomingTxs, peers} waitForTxs log mdbHandle = forever $ do

    (lastblockRef, height)  <- atomically getLastBlockReference

    -- find pending Transaction's to include in the Block
    txs <-
        if waitForTxs then
            atomically $ do
                TransactionQueue txs <- readTVar incomingTxs
                if Set.null txs then
                    retry
                else
                    return txs
        else
            getTransactionQueue <$> readTVarIO incomingTxs

    forkIO $ do
        atomically $ waitingForNewLastBlock lastblockRef
        atomically (readLivelyBlocks blockchainState) >>= log . drawLively (show . blockRef)

    -- TODO: waitingForNewLastBlock returns too often, suss
    -- doMining lastblockRef height (toList txs) `race_` threadDelay 240000000 `race_` atomically (waitingForNewLastBlock lastblockRef)
    doMining lastblockRef height txs `race_` threadDelay 240000000

    where

        -- Calculates the reference to the furthest block in the blockchain - that is furthest leaf from LivelyBlocks 
        -- or LivelyBlocks root if LivelyBlocks is empty (then it is Genesis reference)
        getLastBlockReference :: STM (BlockReference, Integer)
        getLastBlockReference = do
            lively@Lively {root, forest} <- readLivelyBlocks blockchainState
            Fixed fixed <- readFixedBlocks blockchainState
            case getLastBlock lively of
                -- LivelyBlocks is empty.
                Nothing -> return (root,
                    case fixed of
                        [] -> 1
                        b:bs -> 1 + blockBlockHeight b)
                Just lastblock -> return (blockRef lastblock, 1 + blockBlockHeight lastblock)

        waitingForNewLastBlock :: BlockReference -> STM ()
        waitingForNewLastBlock oldRef = do
            (ref, _) <- getLastBlockReference
            when (oldRef == ref) retry


        -- doMining :: BlockReference -> Integer -> t Transaction -> IO ()
        doMining lastblockRef height txs = do
            timestamp <- getCurrentTime
            -- keys for coinbase money
            keys <- generateKeys

            -- mine a block
            let (ownedUTXO@(OwnedUTXO (UTXO txid _ _) _), block) = mineBlock targetHash keys timestamp (toList txs) height lastblockRef

            -- log "Start mining."

            -- forces hash crunching
            evaluate . force $ blockHeader block

            -- Remove transactions used in this block. Note that the block might not end up in the blockchain, todo.
            atomically $ modifyTVar' incomingTxs (`removeUsedTransactions` txs)

            -- collect utxo in wallet:
            log "We mined a coin!"
            let storedTx = StoredTransaction txid (Just $ blockRef block) (Left $ coinbaseTransaction block) Waiting
            case mdbHandle of
                Nothing -> log $ "We throw the coin " <> show txid <> "to thrash."
                Just dbHandle -> do
                    dbres <- dbHandle $ do
                        Wallet.insertTransaction storedTx
                        Wallet.insertOwnedUTXO ownedUTXO
                    either (log . ("mine: Wallet db error:\n" <>) . unpack . pShow) return dbres



            -- Put the newly mined block into blockchain.
            -- Broadcast the block to others.
            join . atomically $ do
                -- Note that this atomical operation is time-consuming.
                lively   <- readLivelyBlocks blockchainState
                fixed@(Fixed fixedb) <- readFixedBlocks blockchainState
                future   <- readFutureBlocks blockchainState
                utxoPool <- readUTXOPool blockchainState

                case updateWithBlock forkMaxDiff targetHash utxoPool block lively future of
                    -- BlockInserted fixed' lively' utxoPool' -> do
                    BlockInserted lively' newfixed -> do
                        writeLivelyBlocks blockchainState lively'
                        writeFixedBlocks blockchainState (Fixed $ newfixed ++ fixedb)
                        writeUTXOPool blockchainState (newfixed2UTXOPoolUpdate newfixed utxoPool)
                        return $
                            -- Broadcast it to others. Block is re-broadcasted only the first time when we add it to blockchain.  
                            broadcastAndUpdatePeers peers (BlockMessage block) (BlockAnswer ReceivedBlock)
                    BLockInsertedLinksToRoot lively' -> do
                        writeLivelyBlocks blockchainState lively'
                        return $
                            -- Broadcast it to others. Block is re-broadcasted only the first time when we add it to blockchain.  
                            broadcastAndUpdatePeers peers (BlockMessage block) (BlockAnswer ReceivedBlock)
                    _ -> return $ log "Warning! Mined block is fucked up."


-- Update Blockchain with the addtion of a given block.
addBlock1 :: ForkMaxDiff -> TargetHash -> (UTXOPool, FixedBlocks, LivelyBlocks, FutureBlocks) -> Block -> (UTXOPool, FixedBlocks, LivelyBlocks, FutureBlocks)
addBlock1 forkMaxDiff targetHash (utxoPool, fixed@(Fixed fixedb), lively, future) block =
    -- try to link a new block to one of the recent blocks
    case updateWithBlock forkMaxDiff targetHash utxoPool block lively future  of
        BlockInserted lively' newfixed -> (newfixed2UTXOPoolUpdate newfixed utxoPool, Fixed (newfixed ++ fixedb), lively', future)
        BLockInsertedLinksToRoot lively'       -> (utxoPool, fixed, lively', future)
        FutureBlock future'   -> (utxoPool, fixed, lively, future')
        _                     -> (utxoPool, fixed, lively, future)


addBlock :: ForkMaxDiff -> TargetHash -> BlockchainData -> Block -> BlockchainData
addBlock forkMaxDiff targetHash (BlockchainData fixed lively future utxoPool) block =
    let (utxoPool, fixed, lively, future) = addBlock1 forkMaxDiff targetHash (utxoPool, fixed, lively, future) block
    in BlockchainData fixed lively future utxoPool

whatsNextBlock :: BlockchainData -> Integer
whatsNextBlock (BlockchainData (Fixed fixed) _ _ _) =
    1 + case fixed of
            []  -> 0
            b:_ -> blockBlockHeight b

-- whatsNextBlock :: BlockchainState -> IO Integer
-- whatsNextBlock blockchainState = do
--     FixedBlocks fixed <- atomically $ readFixedBlocks blockchainState
--     return $ 1 + case fixed of
--                     []  -> 0
--                     b:_ -> blockBlockHeight b

fullNodeCatchUpToBlockchain :: (InMemory.HasLogging appState,
    InMemory.InMemory appState m BlockchainData,
    InMemory.InMemory appState m PeersSet) =>
    ForkMaxDiff -> TargetHash -> appState -> IO ()
fullNodeCatchUpToBlockchain forkMaxDiff targetHash appState = do
    blocks <- catchUpToBlockchain forkMaxDiff targetHash (fmap whatsNextBlock . readMemoryIO) appState
    modifyMemoryIO appState (\bs -> foldl' (addBlock forkMaxDiff targetHash) bs blocks)

-- Validate the block, based on outcome do:
-- - add it to blockchain and broadcast further if it's a new block
-- - add to blockchain but don't broadcast
-- - add to future blocks
-- - ignore if invalid
-- receiveBlock :: ForkMaxDiff -> TargetHash -> BlockchainState -> MsgHandler Block ReceivedBlock
receiveBlock :: (HasLogging appState, InMemory appState m UTXOPool,
    InMemory appState m PeersSet, InMemory appState m BlockchainData,
    InMemory appState m LivelyBlocks,
    InMemory appState m FutureBlocks,
    AppendFixed appState m Block) =>
    ForkMaxDiff -> TargetHash -> appState -> MsgHandler Block ReceivedBlock
receiveBlock forkMaxDiff targetHash appState = MsgHandler $ \block -> do
    -- do the validation etc concurrently not to hold a connection for long

    forkIO . join . runAtomically $ do
        -- Note that this atomical operation is time-consuming. TODO: Benchmark how big of a problem that is.
        lively   <- readMemory appState
        future   <- readMemory appState
        utxoPool <- readMemory appState

        -- try to link a new block to one of the recent blocks
        case updateWithBlock forkMaxDiff targetHash utxoPool block lively future of
            BlockInserted lively' newfixed -> do
                writeMemory appState lively'
                appendFixed appState newfixed
                writeMemory appState (newfixed2UTXOPoolUpdate newfixed utxoPool)
                return $ do
                    logger appState "handler: Received block was inserted into chain."
                    -- Broadcast it to others. Block is re-broadcasted only the first time when we add it to blockchain.  
                    broadcastAndUpdatePeers appState (BlockMessage block) (BlockAnswer ReceivedBlock)
            FutureBlock future'   -> do
                writeMemory appState future'
                return $ do
                    logger appState "handler: Received block inserted into futures waiting list."
                    -- We received a block that doesn't link to a known recent chain. Let's query for new blocks.
                    fullNodeCatchUpToBlockchain forkMaxDiff targetHash appState
            BlockAlreadyInserted -> return $ logger appState "handler: Received block was already present in the chain."
            BlockInvalid         -> return $ logger appState "handler: Received block is invalid."
            BLockInsertedLinksToRoot lively' -> do
                writeMemory appState lively'
                return $ do
                    logger appState "handler: Inserted block linking to genesis."
                    -- Broadcast it to others. Block is re-broadcasted only the first time when we add it to blockchain.  
                    broadcastAndUpdatePeers appState (BlockMessage block) (BlockAnswer ReceivedBlock)

    return ReceivedBlock

-- TODO: Write explicitly _IO versions below.

instance InMemory AppState STM FixedBlocks where
    readMemory = readMemory . getFixed . blockchainState
    writeMemory = writeMemory . getFixed . blockchainState
    modifyMemory = modifyMemory . getFixed . blockchainState

instance InMemory AppState STM LivelyBlocks where
    readMemory = readMemory . getLively . blockchainState
    writeMemory = writeMemory . getLively . blockchainState
    modifyMemory = modifyMemory . getLively . blockchainState

instance InMemory AppState STM UTXOPool where
    readMemory = readMemory . getUTXOPool . blockchainState
    writeMemory = writeMemory . getUTXOPool . blockchainState
    modifyMemory = modifyMemory . getUTXOPool . blockchainState

instance InMemory AppState STM FutureBlocks where
    readMemory = readMemory . getFuture . blockchainState
    writeMemory = writeMemory . getFuture . blockchainState
    modifyMemory = modifyMemory . getFuture . blockchainState

instance InMemory AppState STM PeersSet  where
    readMemory = readMemory . peers
    writeMemory = writeMemory . peers
    modifyMemory = modifyMemory . peers

instance InMemory AppState STM TransactionQueue  where
    readMemory = readMemory . incomingTxs
    writeMemory appState txq =  writeMemory (incomingTxs appState) txq
    modifyMemory appState f = modifyMemory (incomingTxs appState) f


data AppStatePlus = AppStatePlus {
    getAppState :: AppState,
    getLogger   :: String -> IO (),
    getDBPool :: Maybe Pool
}

instance HasLogging AppStatePlus where
    logger = getLogger

instance InMemory AppState STM a => InMemory AppStatePlus STM a where
    readMemory = readMemory . getAppState
    writeMemory = writeMemory . getAppState
    modifyMemory = modifyMemory . getAppState

instance InMemory AppState STM BlockchainData where
    readMemory appState = BlockchainData <$> readMemory appState <*> readMemory appState <*> readMemory appState <*> readMemory appState
    writeMemory appState (BlockchainData a b c d) = writeMemory appState a >> writeMemory appState b >> writeMemory appState c >> writeMemory appState d

instance AppendFixed AppStatePlus STM Block where
    appendFixed appState newfixed = modifyMemory appState ( Fixed . (newfixed ++ ) . getFixedBlocks)

-- instance HasDB AppStatePlus where 
    -- executeDBEither appState = Pool.use (getDBPool appState)

-- MessageHandler for full node. Describes actions on every type of incoming message.
-- 
fullNodeHandler :: (HasLogging appState,
 InMemory
   appState m UTXOPool,
    InMemory appState m PeersSet,
    InMemory appState m FixedBlocks,    -- for now used by answerBlockchainQuery, consider  
    InMemory appState m TransactionQueue,
    InMemory appState m BlockchainData,
    InMemory appState m LivelyBlocks,
    InMemory appState m FutureBlocks, AppendFixed appState m Block) =>
    ForkMaxDiff -> TargetHash -> appState -> MessageHandler
fullNodeHandler forkMaxDiff targetHash =
    combineHandlers (const answerPing) (receiveBlock forkMaxDiff targetHash)  receiveTransaction answerBlockchainQuery answerContactQuery


-- Consider abondoning RunningApp idea. Maybe better withLaunchedApp :: (IO Appstate) -> (AppState -> IO a) -> IO a

-- TODO: Improve and abstract data loading.
-- Load JSON data and save it on quit.
-- Error can be thrown at loading and then just quit.
withLoadSave :: (FromJSON a, ToJSON a) => FilePath -> (Either String (TVar a) -> IO b) -> IO b
withLoadSave fp = bracket
    (do
        eload <- eitherDecodeFileStrict fp
        either (return . Left) (fmap Right . newTVarIO) eload)
    (either (const $ return ()) (readTVarIO >=> encodeFile fp))


runFullNode :: Config -> IO ()
runFullNode config =
    -- TODO: optional cmd arg to load state from save, otherwise only loads

    withLogging (loggingMode config)         $ \log    -> topLevelErrorLog "miner: quits" log $
        withLoadSave (peersFilepath config)      $ \epeers ->
            withLoadSave (blockchainFilepath config) $ \efixed ->

                case liftM2 (,) epeers efixed of
                    Left err -> log err >> exitFailure
                    Right (peers, fixed) -> case walletDatabaseConfig config of
                        Nothing ->  main log peers fixed Nothing
                        Just walletDbConfig -> bracket (acquire walletDbConfig) Pool.release $ \pool ->
                                main log peers fixed (Just pool)

    where
        targetHash = difficultyToTargetHash $ targetDifficulty config
        forkMaxDiff1 = forkMaxDiff config
        mine log appSt = mining forkMaxDiff1 targetHash appSt (minerWaitForTxs config) log
        serverAddr = Address "localhost" (port config)
        runServer log appState = server serverAddr log (toServerHandler (fullNodeHandler forkMaxDiff1 targetHash appState) log)

        main log peers fixed pool = do
            log "app: Loaded peers and fixed blocks."

            blockchainState <- initBlockchainState (blockchainGenesis config) fixed
            appSt        <- initAppState blockchainState peers
            let appState = AppStatePlus appSt log pool

            -- query for blocks after our last block
            forkIO $ fullNodeCatchUpToBlockchain forkMaxDiff1 targetHash appState

            -- forkIO runServer
            -- forkIO mine
            concurrently_ (mine log appSt (fmap Pool.use pool)) (runServer log appState)

            -- return ()


        initBlockchainState :: Genesis -> TVar FixedBlocks -> IO BlockchainState
        initBlockchainState genesis fixed = atomically $ do
            fixed1 <- readTVar fixed
            BlockchainState genesis fixed
                <$> newTVar (Lively {
                        root=case fixed1 of
                            Fixed [] -> shash256 (Left genesis)
                            Fixed (b:bs) -> blockRef b, forest=[]})
                <*> newTVar (Future Map.empty)
                <*> newTVar (collectUTXOs (UTXOPool Map.empty) (getFixedBlocks fixed1))

        initAppState :: BlockchainState -> TVar PeersSet -> IO AppState
        initAppState blockchainState peers =
            AppState blockchainState
                <$> newTVarIO (TransactionQueue Set.empty)
                <*> return peers
                <*> newTVarIO []


-- Launch app, do stuff and live the app running.
-- withAppDo :: Config -> (AppState -> IO ()) -> IO ()
-- withAppDo config action = do
--     runFullNode config >>= \case
--         Nothing -> -- error is logged in runFullNode already
--             return ()
--         Just (RunningApp (appSt, main)) -> do
--             action <- async $ action appSt
--             void $ waitBoth action main

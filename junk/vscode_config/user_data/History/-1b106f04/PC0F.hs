{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
module Wallet.Wallet where
import Hashing (TargetHash, difficultyToTargetHash, shash256)
import BlockChain (ForkMaxDiff, LivelyBlocks, FutureBlocks, Lively (Lively), Future (Future), Fixed (Fixed))
import Network.Socket (ServiceName, Socket)
import Node (withLogging, AppendFixed (appendFixed), PeersSet, Status, generateKeys, broadcastAndUpdatePeers, HasDB (executeDBEither), executeDB, onErrorLogAndNothing, onErrorLogAndQuit, acquire, topLevelErrorLog)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON (parseJSON), eitherDecodeFileStrict, encodeFile, Value, decode)
import Server (Address(Address), server)
import MessageHandlers (toServerHandler)
import Wallet.Node (lightNodeHandler, lightNodeCatchUpToBlockchain, FixedLength (FixedLength))
import BlockType (Genesis, BlockHeader (BlockHeader), Output (Output))
import Control.Concurrent.AdvSTM.TVar
import qualified Hasql.Pool as Pool
import Hasql.Pool (Pool)
import Control.Exception (bracket)
import qualified Data.Map as Map
import Control.Concurrent (forkIO)
import Control.Concurrent.Async (concurrently_)
import InMemory (HasLogging (logger), InMemory (readMemory, writeMemory, modifyMemory))
import Control.Concurrent.AdvSTM (AdvSTM, onCommit, atomically)
import Wallet.Session (addFixedBlockHeader, selectFixedCount, selectStatus, insertTransaction, insertOwnedKeys, selectOwnedByStatus, updateTxStatusByBlock)
import Data.Foldable (for_, Foldable (toList))
import Hasql.Transaction (statement)
import qualified Hasql.Transaction.Sessions as Hasql
import Hasql.Transaction.Sessions (Mode(Write), IsolationLevel (Serializable))
import System.Exit (exitFailure)
import Control.Monad ((>=>))
import qualified Data.ByteString as B
import Data.Text (Text)
-- import Text.Pretty.Simple (pShow)
import Data.Aeson.Types (Parser)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Configs (PoolSettings (..), ConnectionSettings (..), WalletConfig(..), NodeConfig(..), BlockchainConfig(..), LoggingMode)
import Hasql.Session (Session)
import Hasql.Connection (settings)
import Wallet.Repl (serveRepl, CommandR(..), AddCoinResponse (AddCoinFail, AddCoinSuccess), AddTransactionResponse (AddTransactionSuccess, AddTransactionFail), SendTransactionResponse (SendTransactionFailure, NotEnoughFunds, SendedTransaction), StatusResponse (GetStatusFailure, StatusIs), GetCoinsResponse (GetCoinsFailure))
import Wallet.Type (StoredTransaction(StoredTransaction), Status (Waiting, Validated))
import BlockCreation (Keys(Keys), OwnedUTXO (OwnedUTXO), createSendingTransaction)
import BlockValidation (UTXO(UTXO))
import MessageType (Answer(TransactionAnswer), Message (TransactionMessage), ReceivedTransaction (ReceivedTransaction))

data BlockchainState = BlockchainState {
    getGenesis :: Genesis,
    getLively :: TVar (Lively BlockHeader),
    getFuture :: TVar (Future BlockHeader),
    getFixedLength :: TVar FixedLength
    -- Fixed is kept in database.
}

data AppState = AppState {
    blockchainState :: BlockchainState,
    getLogger :: String -> IO (),
    getDBPool :: Pool,
    getContacts :: TVar PeersSet
}

-- Option1 : 
--  - appState contains (Session a -> IO a), but it's valid only in some scope (same as logging but whatever)
-- Option2 : 
--  - appState contains pool
-- Option 3:
--  - withPool :: HasDB appState => PoolSettings -> (appState -> IO a) -> appState -> IO a  

instance InMemory AppState AdvSTM (Future BlockHeader) where
    readMemory = readMemory . getFuture . blockchainState
    writeMemory = writeMemory . getFuture . blockchainState
    modifyMemory = modifyMemory . getFuture . blockchainState

instance InMemory AppState AdvSTM (Lively BlockHeader) where
    readMemory = readMemory . getLively . blockchainState
    writeMemory = writeMemory . getLively . blockchainState
    modifyMemory = modifyMemory . getLively . blockchainState

instance InMemory AppState AdvSTM PeersSet where
    readMemory = readMemory . getContacts
    writeMemory = writeMemory . getContacts
    modifyMemory = modifyMemory . getContacts

instance InMemory AppState AdvSTM FixedLength  where
    readMemory = readMemory . getFixedLength . blockchainState
    writeMemory = writeMemory . getFixedLength . blockchainState
    modifyMemory = modifyMemory . getFixedLength . blockchainState


instance AppendFixed AppState AdvSTM BlockHeader where
    appendFixed appState newfixed = 
        onCommit . executeDB appState . for_ newfixed $ \fb -> Hasql.transaction Serializable Write $ do 
            addFixedBlockHeader fb
            updateTxStatusByBlock Validated (shash256 $ Right fb)

instance HasDB AppState where 
    executeDBEither appState = Pool.use (getDBPool appState)  
-- Turned out in one place I do want to recover from error, in repl where we execute user provided commands.

instance HasLogging AppState where
    logger = getLogger 

-- Version for AdvSTM.
-- Load JSON data and save it on quit.
-- Error can be thrown at loading and then just quit.
withLoadSave :: (FromJSON a, ToJSON a) => FilePath -> (Either String (TVar a) -> IO b) -> IO b
withLoadSave fp = bracket
    (do
        eload <- eitherDecodeFileStrict fp
        either (return . Left) (fmap Right . newTVarIO) eload)
    (either (const $ return ()) (atomically . readTVar >=> encodeFile fp))

-- Execute user's wallet command provided with a handle to db pool that logs the error and projects to Nothing.
replHandler :: (InMemory appState m PeersSet, HasLogging appState) => 
    appState -> (forall a . Session a -> IO (Maybe a)) -> CommandR r-> IO r
-- replHandler :: appState -> CommandR r-> IO r
replHandler appState usePool (AddCoin (OwnedUTXO (UTXO txid vout (Output cents pub_addr)) (Keys pub priv))) = do 
    -- we discard Output information from OwnedUTXO 
    m <- usePool $ insertOwnedKeys txid (fromInteger vout) pub priv cents pub_addr
    case m of 
        Nothing -> logger appState "repl: Added coin." >>return AddCoinFail
        Just () -> logger appState "repl: Couldn't add coin. Fails." >> return AddCoinSuccess 

replHandler appState usePool (AddTransaction tx blockref) = do 
    m <- usePool $ insertTransaction (StoredTransaction (shash256 tx) blockref tx Waiting)
    case m of 
        Nothing -> logger appState "repl: Added transaction." >> return AddTransactionSuccess 
        Just () -> logger appState "repl: Couldn't add transaction. Fails." >> return AddTransactionFail

replHandler appState usePool (SendTransaction recipient n) = do 
    newkeys <- generateKeys
    res <- usePool (db newkeys)
    case res of 
        -- db session error
        Nothing -> logger appState "repl: Couldn't create a transaction due to db error. Fails." >> return SendTransactionFailure
        -- not enough founds
        Just Nothing -> logger appState "repl: Couldn't create a transaction. Not enough funds. Fails." >> return NotEnoughFunds 
        Just (Just newtx) -> do
            forkIO $ broadcastAndUpdatePeers appState (TransactionMessage newtx) (TransactionAnswer ReceivedTransaction)   -- broadcast transaction
            logger appState "repl: Created and sended transaction."
            return SendedTransaction

    where 
        db newkeys = do
            utxos <- selectOwnedByStatus Validated
            case createSendingTransaction (toList utxos) newkeys recipient n of
                Nothing -> return Nothing
                -- Keys here are the same newkeys
                -- used up transactions seem irrelevant here? should remove them only when tx added to fixed
                Just (OwnedUTXO (UTXO txid vout out) (Keys pub priv), _, newtx) -> do
                    -- Here only remove
                    insertTransaction $ StoredTransaction txid Nothing (Right newtx) Waiting
                    insertOwnedKeys txid (fromInteger vout) pub priv
                    return $ Just newtx

replHandler appState usePool GetCoins = do 
    return GetCoinsFailure


replHandler appState usePool (GetStatus txid) = do 
    ms <- usePool $ selectStatus txid
    case ms of 
        Nothing -> return GetStatusFailure
        Just status -> logger appState "repl: Returned queried transaction status." >> return (StatusIs txid status)

runWallet :: WalletConfig  -> IO ()
runWallet config =
    withLogging (loggingMode $ nodeConfig config) $ \log -> topLevelErrorLog "wallet: quits." log $
        withLoadSave (peersFilepath $ nodeConfig config) $ \case
            Left e -> log e >> exitFailure
            Right peers ->
                bracket (acquire $ databaseConfig config) Pool.release $ \pool ->
                    main log pool peers

    where
        targetHash = difficultyToTargetHash . targetDifficulty . blockchainConfig $ config
        forkMaxDiff1 = forkMaxDiff $ blockchainConfig config
        serverAddr = Address "localhost" (port $ nodeConfig config)
        replAddr   = Address "localhost" (replPort config)
        runServer log appState = server serverAddr log (toServerHandler (lightNodeHandler forkMaxDiff1 targetHash appState) log)
        runRepl log appState pool =
            serveRepl replAddr log (replHandler appState (onErrorLogAndNothing "repl: Execution of user command ended with:\n" log (Pool.use pool)))

        main log pool peers = do
            log "wallet: Started."

            -- Read FixedLength
            fixedLength <- onErrorLogAndQuit log (Pool.use pool) selectFixedCount

            blockchainState <- initBlockchainState (blockchainGenesis $ blockchainConfig config) fixedLength
            let appState = AppState blockchainState log pool peers

            -- print lively blocks
            -- forkIO $ logLivelyOnChange "lively_wallet_monitor" 
            --     (getLively blockchainState) 
            --     (\b -> show (previousHash b, shash256 $ (Right b :: Either Genesis BlockHeader)))


            -- query for blocks after our last block
            forkIO $ lightNodeCatchUpToBlockchain forkMaxDiff1 targetHash appState

            -- forkIO runServer
            -- forkIO mine
            log $ "wallet: Serving repl on port: " <> replPort config <> "."
            concurrently_ (runRepl log appState pool) (runServer log appState)

        initBlockchainState genesis fixedLength =
            BlockchainState genesis
                <$> newTVarIO (Lively (shash256 $ Left genesis) [])
                <*> newTVarIO (Future Map.empty)
                <*> newTVarIO (FixedLength fixedLength)

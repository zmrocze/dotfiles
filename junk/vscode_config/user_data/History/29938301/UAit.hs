
module Test.Plutip.Internal.DistributeFunds where

-- module Test.Plutip.Internal.BotPlutusInterface.Wallet (
--   BpiWallet (..),
--   addSomeWallet,
--   addSomeWalletDir,
--   eitherAddSomeWallet,
--   eitherAddSomeWalletDir,
--   mkMainnetAddress,
--   cardanoMainnetAddress,
--   ledgerPaymentPkh,
--   showAddress,
-- ) where

import Cardano.BM.Data.Tracer (nullTracer)
import Cardano.Wallet.Primitive.Types.Coin (Coin (Coin))

-- import Cardano.Wallet.Shelley.Launch.Cluster (
--   sendFaucetFundsTo,
--  )

-- import Data.Aeson.Extras (encodeByteString)
import Numeric.Positive (Positive)
import System.FilePath ((</>))
import Test.Plutip.Internal.Types (ClusterEnv, nodeSocket, supportDir)

import Test.Plutip.Internal.Keys (KeyPair, mainnetAddress)


-- import Cardano.Wallet.Api.Types
--     ( decodeAddress )



-- import Cardano.Api
--     ( parseLoggingSeverity )
-- import Cardano.CLI ( parseLoggingSeverity )
import Cardano.Launcher.Node
    ( CardanoNodeConn
    
    
    
    )
-- import Cardano.Wallet.Api.Http.Server
--     ( Listen (..) )
-- import Cardano.Wallet.Api
--     ( ApiEra (..) )
-- import Cardano.Wallet.Api.Types
--     ( ApiEra (..) 
    -- , DecodeAddress (..), HealthStatusSMASH (..) )
import Cardano.Wallet.Primitive.AddressDerivation
    ( hex )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (Coin, unCoin) )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( AssetId (AssetId), TokenBundle (TokenBundle) )
import Cardano.Wallet.Primitive.Types.TokenPolicy
    ( TokenName (UnsafeTokenName) )
import Cardano.Wallet.Primitive.Types.TokenQuantity
    ( TokenQuantity (TokenQuantity) )
-- import Cardano.Wallet.Shelley.Launch
--     ( TempDirLog, envFromText, lookupEnvNonEmpty )
import Control.Monad
    ( forM, forM_, when )
import Control.Tracer
    ( Tracer (..) )
import Crypto.Hash.Utils
    ( blake2b256 )
import Data.ByteString.Base58
    ( bitcoinAlphabet, decodeBase58 )
import Data.List
    ( intercalate, nub )
import Data.Maybe
    ( fromMaybe )
import Data.Text.Class
    ( ToText(toText) )
import System.IO.Unsafe
    ( unsafePerformIO )
import UnliftIO.MVar
    ( MVar, modifyMVar, newMVar )

import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Cardano.Wallet.Primitive.Types.TokenMap as TokenMap
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Test.Plutip.Internal.Cluster (ClusterLog, getShelleyTestDataPath, signTx)

fundKey :: ClusterEnv -> KeyPair -> [Positive] -> IO ()
fundKey cEnv keys funds = do
  let fundAddress = mainnetAddress keys
      toAmt = Coin . fromIntegral
  sendFaucetFundsTo
    nullTracer -- todo: fix tracer to be not `nullTracer`
    (nodeSocket cEnv)
    (supportDir cEnv)
    [(fundAddress, toAmt v) | v <- funds]

sendFaucetFundsTo
    :: Tracer IO ClusterLog
    -> CardanoNodeConn
    -> FilePath
    -> [(String, Coin)]
    -> IO ()
sendFaucetFundsTo tr conn dir targets = batch 80 targets $
    sendFaucet tr conn dir "ada" . map coinBundle
  where
    coinBundle = fmap (\c -> (TokenBundle.fromCoin c, []))

-- | Create transactions to fund the given faucet addresses with Ada and assets.
--
-- Beside the 'TokenBundle' of Ada and assets, there is a list of
-- @(signing key, verification key hash)@ pairs needed to sign the
-- minting transaction.
sendFaucetAssetsTo
    :: Tracer IO ClusterLog
    -> CardanoNodeConn
    -> FilePath
    -> Int -- ^ batch size
    -> [(String, (TokenBundle, [(String, String)]))] -- ^ (address, assets)
    -> IO ()
sendFaucetAssetsTo tr conn dir batchSize targets = do
    era <- getClusterEra dir
    when (era >= MaryHardFork) $
        batch batchSize targets $ sendFaucet tr conn dir "assets"

-- | Build, sign, and send a batch of faucet funding transactions using
-- @cardano-cli@. This function is used by 'sendFaucetFundsTo' and
-- 'sendFaucetAssetsTo'.
sendFaucet
    :: Tracer IO ClusterLog
    -> CardanoNodeConn
    -> FilePath
    -> String -- ^ label for logging
    -> [(String, (TokenBundle, [(String, String)]))]
    -> IO ()
sendFaucet tr conn dir what targets = do
    (faucetInput, faucetPrv) <- takeFaucet
    let file = dir </> "faucet-tx.raw"

    let mkOutput addr (TokenBundle (Coin c) tokens) =
            [ "--tx-out"
            , unwords $ [ addr, show c, "lovelace"] ++
                map (("+ " ++) . cliAsset) (TokenMap.toFlatList tokens)
            ]
        cliAsset (aid, (TokenQuantity q)) = unwords [show q, cliAssetId aid]
        cliAssetId (AssetId pid (UnsafeTokenName name)) = mconcat
            [ T.unpack (toText pid)
            , if B8.null name then "" else "."
            , B8.unpack (hex name)
            ]
        mkMint [] = []
        mkMint assets = ["--mint", intercalate " + " (map cliAsset assets)]

    let total = fromIntegral $ sum $
            map (unCoin . TokenBundle.getCoin . fst . snd) targets
    when (total > faucetAmt) $ error "sendFaucetFundsTo: too much to pay"

    let targetAssets = concatMap (snd . TokenBundle.toFlatList . fst . snd) targets

    scripts <- forM (nub $ concatMap (map snd . snd . snd) targets) $
        writeMonetaryPolicyScriptFile dir

    cli tr $
        [ "transaction", "build-raw"
        , "--tx-in", faucetInput
        , "--ttl", "6000000"
            -- Big enough to allow minting in the actual integration tests,
            -- before the wallet API supports it.
        , "--fee", show (faucetAmt - total)
        , "--out-file", file
        ] ++
        concatMap (uncurry mkOutput . fmap fst) targets ++
        mkMint targetAssets ++
        (concatMap (\f -> ["--minting-script-file", f]) scripts)

    policyKeys <- forM (nub $ concatMap (snd . snd) targets) $
        \(skey, keyHash) -> writePolicySigningKey dir keyHash skey

    tx <- signTx tr dir file (faucetPrv:policyKeys)
    submitTx tr conn (what ++ " faucet tx") tx

batch :: Int -> [a] -> ([a] -> IO b) -> IO ()
batch s xs = forM_ (group s xs)
  where
    -- TODO: Use split package?
    -- https://stackoverflow.com/questions/12876384/grouping-a-list-into-lists-of-n-elements-in-haskell
    group :: Int -> [a] -> [[a]]
    group _ [] = []
    group n l
      | n > 0 = (take n l) : (group n (drop n l))
      | otherwise = error "Negative or zero n"

-- | Hard-wired faucets referenced in the genesis file. Purpose is simply to
-- fund some initial transaction for the cluster. Faucet have plenty of money to
-- pay for certificates and are intended for a one-time usage in a single
-- transaction.
takeFaucet :: IO (String, String)
takeFaucet = do
    i <- modifyMVar faucetIndex (\i -> pure (i+1, i))
    source <- getShelleyTestDataPath
    let basename = source </> "faucet-addrs" </> "faucet" <> show i
    base58Addr <- BS.readFile $ basename <> ".addr"
    let addr = fromMaybe (error $ "decodeBase58 failed for " ++ show base58Addr)
            . decodeBase58 bitcoinAlphabet
            . T.encodeUtf8
            . T.strip
            $ T.decodeUtf8 base58Addr

    let txin = B8.unpack (hex $ blake2b256 addr) <> "#0"
    let signingKey = basename <> ".shelley.key"
    pure (txin, signingKey)

-- | List of faucets also referenced in the shelley 'genesis.yaml'
faucetIndex :: MVar Int
faucetIndex = unsafePerformIO $ newMVar 1
{-# NOINLINE faucetIndex #-}
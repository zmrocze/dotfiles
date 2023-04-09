module Test.Plutip.LocalCluster (
  BpiWallet,
  RetryDelay,
  addSomeWallet,
  ada,
  waitSeconds,
  mkMainnetAddress,
  cardanoMainnetAddress,
  ledgerPaymentPkh,
  withCluster,
  withConfiguredCluster,
  startCluster,
  stopCluster,
  -- plutusValueFromWallet,
) where

import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (MonadReader (ask), ReaderT, ask)
import Data.Bifunctor (second)
import Data.Default (def)
import Data.List.NonEmpty (NonEmpty)
-- import Ledger (Value)
import Numeric.Natural (Natural)
import Numeric.Positive (Positive)
import Test.Plutip.Config (PlutipConfig)
import Test.Plutip.Contract.Init (ada)
import Test.Plutip.Contract.Types (
  TestWallet (twInitDistribuition),
  TestWallets (unTestWallets),
 )
import Test.Plutip.Internal.BotPlutusInterface.Wallet (
  BpiWallet,
  addSomeWallet,
  cardanoMainnetAddress,
  ledgerPaymentPkh,
  mkMainnetAddress,
 )
-- import Test.Plutip.Internal.Cluster.Extra.Types (ecSlotLength)
import Test.Plutip.Internal.LocalCluster (startCluster, stopCluster)
import Test.Plutip.Internal.Types (ClusterEnv)
-- import Test.Plutip.Tools.CardanoApi (CardanoApiError)
-- import Test.Plutip.Tools.ChainIndex qualified as CI
import Test.Tasty (testGroup, withResource)
import Test.Tasty.Providers (TestTree)
import Test.Plutip.Tools.CardanoApi (awaitUtxosNumber)

-- | Awaiting via `threadDelay`
waitSeconds :: Natural -> ReaderT ClusterEnv IO ()
waitSeconds n = liftIO $ threadDelay (fromEnum n * 1_000_000)

-- | Spin up a local cluster and create a test group with the contracts inside it.
-- The cluster is reused by all the test cases, but the wallets are isolated, so contracts won't
-- depend on each other (note that time related issues might still occur).
-- Uses default `PlutipConfig`.
--
-- = Usage
-- > test :: TestTree
-- > test =
-- >   withCluster
-- >     "Tests with local cluster"
-- >     [ shouldSucceed "Get utxos" (initAda 100) $ const getUtxos
-- >     ...
--
-- @since 0.2
withCluster ::
  ((ClusterEnv, NonEmpty BpiWallet) -> IO a) ->
  IO a
withCluster = withConfiguredCluster def

-- | Spin up a local cluster and create a test group with the contracts inside it.
-- The cluster is reused by all the test cases, but the wallets are isolated, so contracts won't
-- depend on each other (note that time related issues might still occur).
--
-- = Usage
-- > test :: TestTree
-- > test =
-- >     let myConfig = PlutipConfig ...
-- >     withConfiguredCluster myConfig
-- >     "Tests with local cluster"
-- >     [ shouldSucceed "Get utxos" (initAda 100) $ const getUtxos
-- >     ...
--
-- @since 0.2
withConfiguredCluster ::
  PlutipConfig ->
  [[Positive]] ->
  ((ClusterEnv, NonEmpty BpiWallet) -> IO a) ->
  IO a
withConfiguredCluster conf twalls action =
  startCluster conf setup
  stopCluster cenv
  where
    setup :: ReaderT ClusterEnv IO (ClusterEnv, [NonEmpty BpiWallet])
    setup = do
      wallets <-
        traverse
          (traverse addSomeWallet)
          twalls
      awaitUtxosNumber cenv numUtxos wallets
      pure (cenv, wallets)

    numUtxos = length $ concat twalls   
    -- awaitFunds ws delay = do
    --   let lastWallet = NE.last $ last ws
    --   liftIO $ putStrLn "Waiting till all wallets will be funded to start tests..."
    --   -- TODO: Switch to node queries 
    --   undefined $ CI.awaitWalletFunded lastWallet delay

type RetryDelay = Positive

imap :: (Int -> a -> b) -> [a] -> [b]
imap fn = zipWith fn [0 ..]

-- UNUSED
-- -- Get total `Value` of all UTxOs at `BpiWallet` address.
-- plutusValueFromWallet ::
--   MonadIO m =>
--   BpiWallet ->
--   ReaderT ClusterEnv m (Either CardanoApiError Value)
-- plutusValueFromWallet bw = do
--   cEnv <- ask
--   liftIO . plutusValueFromAddress cEnv . cardanoMainnetAddress $ bw

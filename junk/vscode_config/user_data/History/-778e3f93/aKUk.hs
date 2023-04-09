module Test.Plutip.LocalCluster (
  BpiWallet,
  RetryDelay,
  addSomeWallet,
  ada,
  mkMainnetAddress,
  cardanoMainnetAddress,
  ledgerPaymentPkh,
  withCluster,
  withConfiguredCluster,
  startCluster,
  stopCluster,
  -- plutusValueFromWallet,
) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT (runReaderT))
import Data.Default (def)
import Numeric.Positive (Positive)
import Test.Plutip.Config (PlutipConfig)
import Test.Plutip.Internal.BotPlutusInterface.Wallet (
  BpiWallet,
  addSomeWallet,
  cardanoMainnetAddress,
  ledgerPaymentPkh,
  mkMainnetAddress,
 )
-- import Test.Plutip.Internal.Cluster.Extra.Types (ecSlotLength)
-- import Test.Plutip.Internal.Cluster.Extra.Types (ecSlotLength)
import Test.Plutip.Internal.LocalCluster (startCluster, stopCluster)
import Test.Plutip.Internal.Types (ClusterEnv)
-- import Test.Plutip.Tools.CardanoApi (CardanoApiError)
-- import Test.Plutip.Tools.ChainIndex qualified as CI
import Test.Plutip.Tools.CardanoApi (awaitUtxosNumber)

-- | Library functions works with amounts in `Lovelace`.
-- This function helps to specify amounts in `Ada` easier.
ada :: Positive -> Positive
ada = (* 1_000_000)

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
  [[Positive]] ->
  ([BpiWallet] -> IO a) ->
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
  ([BpiWallet] -> IO a) ->
  IO a
withConfiguredCluster conf twalls action = do
  (ref, cenv) <- startCluster conf pure
  wallets <- setup cenv
  a <- action wallets 
  stopCluster ref
  pure a

  where
    setup :: ClusterEnv -> IO [BpiWallet]
    setup cenv = do
      wallets <- traverse addSomeWallet twalls `runReaderT` cenv
      _ <- liftIO $ awaitUtxosNumber cenv (map cardanoMainnetAddress wallets) numUtxos
      -- TODO: report error on error
      pure wallets

    numUtxos = length $ concat twalls

type RetryDelay = Positive

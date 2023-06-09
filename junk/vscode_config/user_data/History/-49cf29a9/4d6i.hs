module Api.Handlers (
  startClusterHandler,
  stopClusterHandler,
) where

import Cardano.Api (serialiseToCBOR)
import Cardano.Launcher.Node (nodeSocketFile)
import Test.Plutip.Tools.CardanoApi qualified as Tools

import Control.Concurrent.MVar (isEmptyMVar, putMVar, takeMVar)
import Cardano.Wallet.Shelley.Launch.Cluster (RunningNode (RunningNode))
import Control.Arrow (left)
import Control.Concurrent.MVar (isEmptyMVar, putMVar, tryTakeMVar)
import Control.Monad (unless)
import Control.Monad.Except (ExceptT (ExceptT), runExceptT, throwError)
import Control.Monad.Extra (unlessM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, ask, asks)
import Data.ByteString.Base16 qualified as Base16
import Data.Default (def)
import Data.Foldable (for_)
import Data.Text.Encoding qualified as Text
import Data.Traversable (for)
import System.Directory (doesFileExist)
import System.FilePath (replaceFileName)
import Test.Plutip.Config (
  ChainIndexMode (NotNeeded),
  PlutipConfig (extraConfig),
  chainIndexMode,
  relayNodeLogs,
 )
import Test.Plutip.Internal.BotPlutusInterface.Setup (keysDir)
import Test.Plutip.Internal.BotPlutusInterface.Wallet (
  BpiWallet (signKey),
  addSomeWallet,
  cardanoMainnetAddress,
 )
import Test.Plutip.Internal.Cluster (RunningNode (RunningNode))
import Test.Plutip.Internal.Cluster.Extra.Types (ExtraConfig (ExtraConfig, ecSlotLength))
import Test.Plutip.Internal.LocalCluster (startCluster, stopCluster)
import Test.Plutip.Internal.Types (ClusterEnv (plutipConf, runningNode))
import Test.Plutip.Internal.Types (ClusterEnv (runningNode))
import Test.Plutip.LocalCluster (cardanoMainnetAddress)
import Test.Plutip.Tools.CardanoApi (awaitWalletFunded)
import Types (
  AppM,
  ClusterStartupFailureReason (
    ClusterIsRunningAlready,
    NegativeLovelaces,
    NodeConfigNotFound,
    WaitingForFundedWalletsFailed
  ),
  ClusterStartupParameters (
    ClusterStartupParameters,
    keysDirectory,
    nodeConfigPath,
    nodeSocketPath,
    privateKeys
  ),
  Env (status),
  Lovelace (unLovelace),
  PrivateKey,
  ServerOptions (ServerOptions, nodeLogs),
  StartClusterRequest (
    StartClusterRequest,
    epochSize,
    keysToGenerate,
    slotLength
  ),
  StartClusterResponse (
    ClusterStartupFailure,
    ClusterStartupSuccess
  ),
  StopClusterRequest (StopClusterRequest),
  StopClusterResponse (StopClusterFailure, StopClusterSuccess),
 )

startClusterHandler :: ServerOptions -> StartClusterRequest -> AppM StartClusterResponse
startClusterHandler
  ServerOptions {nodeLogs}
  StartClusterRequest {slotLength, epochSize, keysToGenerate} = interpret $ do
    -- Check that lovelace amounts are positive
    for_ keysToGenerate $ \lovelaceAmounts -> do
      for_ lovelaceAmounts $ \lovelaces -> do
        unless (unLovelace lovelaces > 0) $ do
          throwError NegativeLovelaces
    statusMVar <- asks status
    isClusterDown <- liftIO $ isEmptyMVar statusMVar
    unless isClusterDown $ throwError ClusterIsRunningAlready
    let extraConf = ExtraConfig slotLength epochSize
        cfg = def {relayNodeLogs = nodeLogs, chainIndexMode = NotNeeded, extraConfig = extraConf}

    (statusTVar, (clusterEnv, wallets)) <- liftIO $ startCluster cfg setup
    liftIO $ putMVar statusMVar statusTVar
    waitForFundingTxs clusterEnv wallets
    let nodeConfigPath = getNodeConfigFile clusterEnv
    -- safeguard against directory tree structure changes
    unlessM (liftIO $ doesFileExist nodeConfigPath) $ throwError NodeConfigNotFound
    pure $
      ClusterStartupSuccess $
        ClusterStartupParameters
          { privateKeys = getWalletPrivateKey <$> wallets
          , nodeSocketPath = getNodeSocketFile clusterEnv
          , nodeConfigPath = nodeConfigPath
          , keysDirectory = keysDir clusterEnv
          }
    where
      setup :: ReaderT ClusterEnv IO (ClusterEnv, [BpiWallet])
      setup = do
        wallets <- do
          for keysToGenerate $ \lovelaceAmounts -> do
            addSomeWallet (fromInteger . unLovelace <$> lovelaceAmounts)
        liftIO $ putStrLn "Waiting for wallets to be funded..."
        awaitFunds wallets (ecSlotLength $ extraConfig $ plutipConf env)
        pure (env, wallets)
      getNodeSocketFile (runningNode -> RunningNode conn _ _ _) = nodeSocketFile conn
      getNodeConfigFile =
        -- assumption is that node.config lies in the same directory as node.socket
        flip replaceFileName "node.config" . getNodeSocketFile

      getWalletPrivateKey :: BpiWallet -> PrivateKey
      getWalletPrivateKey = Text.decodeUtf8 . Base16.encode . serialiseToCBOR . signKey
      interpret = fmap (either ClusterStartupFailure id) . runExceptT

      -- waits for the last wallet to be funded
      awaitFunds ws delay = do
        let lastWalletPkh = cardanoMainnetAddress $ last ws
        liftIO $ putStrLn "Waiting till all wallets will be funded..."
        Tools.awaitAddressFunded lastWalletPkh delay

stopClusterHandler :: StopClusterRequest -> AppM StopClusterResponse
stopClusterHandler StopClusterRequest = do
  statusMVar <- asks status
  maybeClusterStatus <- liftIO $ tryTakeMVar statusMVar
  case maybeClusterStatus of
    Nothing -> pure $ StopClusterFailure "Cluster is not running"
    Just statusTVar -> do
      liftIO $ stopCluster statusTVar
      pure StopClusterSuccess

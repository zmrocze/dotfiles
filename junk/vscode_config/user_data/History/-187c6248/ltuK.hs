module Api.Handlers (
  startClusterHandler,
  stopClusterHandler,
) where

import Cardano.Launcher.Node (nodeSocketFile)

import Control.Concurrent.MVar (isEmptyMVar, putMVar, tryTakeMVar)
import Control.Monad (unless)
import Control.Monad.Except (runExceptT, throwError)
import Control.Monad.Extra (unlessM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Default (def)
import Data.Foldable (for_)
import Plutip.Cluster (startFundedCluster, stopCluster)
import Plutip.Config (
  ExtraConfig (ExtraConfig),
  PlutipConfig (extraConfig),
 )
import Plutip.Keys (signKeyCBORHex)
import Plutip.Types (ClusterEnv (runningNode), RunningNode (RunningNode), keysDir)
import System.Directory (doesFileExist)
import System.FilePath (replaceFileName)
import Types (
  AppM,
  ClusterStartupFailureReason (
    ClusterIsRunningAlready,
    NegativeLovelaces,
    NodeConfigNotFound
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
  ServerOptions,
  StartClusterRequest (
    StartClusterRequest,
    epochSize,
    keysToGenerate,
    maxTxSize,
    raiseExUnitsToMax,
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
  _
  StartClusterRequest
    { keysToGenerate
    , slotLength
    , epochSize
    , maxTxSize
    , raiseExUnitsToMax
    } = interpret $ do
    -- Check that lovelace amounts are positive
    for_ keysToGenerate $ \lovelaceAmounts -> do
      for_ lovelaceAmounts $ \lovelaces -> do
        unless (unLovelace lovelaces > 0) $ do
          throwError NegativeLovelaces
    statusMVar <- asks status
    isClusterDown <- liftIO $ isEmptyMVar statusMVar
    unless isClusterDown $ throwError ClusterIsRunningAlready
<<<<<<< HEAD
    let cfg = def {extraConfig = extraConf}
        keysToGenerate' = map fromIntegral <$> keysToGenerate
    (statusTVar, (clusterEnv, keys)) <- liftIO $ startFundedCluster cfg keysToGenerate' (curry pure)
=======
    let cfg = def {relayNodeLogs = nodeLogs, chainIndexMode = NotNeeded, extraConfig = extraConf}

    (statusTVar, (clusterEnv, wallets)) <- liftIO $ startCluster cfg setup
>>>>>>> maxTxSize
    liftIO $ putMVar statusMVar statusTVar

    let nodeConfigPath = getNodeConfigFile clusterEnv
    -- safeguard against directory tree structure changes
    unlessM (liftIO $ doesFileExist nodeConfigPath) $ throwError NodeConfigNotFound
    pure $
      ClusterStartupSuccess $
        ClusterStartupParameters
          { privateKeys = signKeyCBORHex <$> keys
          , nodeSocketPath = getNodeSocketFile clusterEnv
          , nodeConfigPath = nodeConfigPath
          , keysDirectory = keysDir clusterEnv
          }
    where
<<<<<<< HEAD
=======
      setup :: ReaderT ClusterEnv IO (ClusterEnv, [BpiWallet])
      setup = do
        env <- ask
        wallets <- do
          for keysToGenerate $ \lovelaceAmounts -> do
            addSomeWallet (fromInteger . unLovelace <$> lovelaceAmounts)
        return (env, wallets)

      -- wait for confirmation of funding txs, throw the first error if there's any
      waitForFundingTxs clusterEnv wallets extraConfig = do
        res <- for wallets $ \w ->
          awaitWalletFunded clusterEnv (cardanoMainnetAddress w) extraConfig
        return $
          firstJust
            ( \case
                Left (AwaitingCapiError e) -> Just $ show e
                Left AwaitingTimeoutError -> Nothing
                Right () -> Nothing
            )
            res

>>>>>>> maxTxSize
      getNodeSocketFile (runningNode -> RunningNode conn _ _ _) = nodeSocketFile conn
      getNodeConfigFile =
        -- assumption is that node.config lies in the same directory as node.socket
        flip replaceFileName "node.config" . getNodeSocketFile
<<<<<<< HEAD
=======

      getWalletPrivateKey :: BpiWallet -> PrivateKey
      getWalletPrivateKey = Text.decodeUtf8 . Base16.encode . serialiseToCBOR . signKey
>>>>>>> maxTxSize
      interpret = fmap (either ClusterStartupFailure id) . runExceptT

      extraConf :: ExtraConfig
      extraConf =
        let defConfig = def
         in ExtraConfig
              (fromMaybe (ecSlotLength defConfig) slotLength)
              (fromMaybe (ecEpochSize defConfig) epochSize)
              (fromMaybe (ecMaxTxSize defConfig) maxTxSize)
              (fromMaybe (ecRaiseExUnitsToMax defConfig) raiseExUnitsToMax)

stopClusterHandler :: StopClusterRequest -> AppM StopClusterResponse
stopClusterHandler StopClusterRequest = do
  statusMVar <- asks status
  maybeClusterStatus <- liftIO $ tryTakeMVar statusMVar
  case maybeClusterStatus of
    Nothing -> pure $ StopClusterFailure "Cluster is not running"
    Just statusTVar -> do
      liftIO $ stopCluster statusTVar
      pure StopClusterSuccess

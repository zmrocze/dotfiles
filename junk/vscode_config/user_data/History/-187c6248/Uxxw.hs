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
import System.Directory (doesFileExist)
import System.FilePath (replaceFileName)
import Test.Plutip.Config (
  PlutipConfig (extraConfig)
 )
import Test.Plutip.Internal.BotPlutusInterface.Setup (keysDir)
import Test.Plutip.Internal.Cluster (RunningNode (RunningNode))
import Test.Plutip.Internal.Cluster.Extra.Types (ExtraConfig (ExtraConfig, ecSlotLength))
import Test.Plutip.Internal.LocalCluster (stopCluster, startFundedCluster)
import Test.Plutip.Internal.Types (ClusterEnv (plutipConf, runningNode))
import Types (
  AppM,
  ClusterStartupFailureReason (
    ClusterIsRunningAlready,
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
import Test.Plutip.Internal.Keys (signKeyCBORHex)

startClusterHandler :: ServerOptions -> StartClusterRequest -> AppM StartClusterResponse
startClusterHandler
  _
  StartClusterRequest {slotLength, epochSize, keysToGenerate} = interpret $ do
    statusMVar <- asks status
    isClusterDown <- liftIO $ isEmptyMVar statusMVar
    unless isClusterDown $ throwError ClusterIsRunningAlready
    let extraConf = ExtraConfig slotLength epochSize
        cfg = def {extraConfig = extraConf}
        keysToGenerate' = map fromIntegral <$> keysToGenerate -- integers are positive, checked in json parsing

    (statusTVar, (clusterEnv, keys)) <- liftIO $ startFundedCluster cfg keysToGenerate' (curry pure)
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

      getNodeSocketFile (runningNode -> RunningNode conn _ _ _) = nodeSocketFile conn
      getNodeConfigFile =
        -- assumption is that node.config lies in the same directory as node.socket
        flip replaceFileName "node.config" . getNodeSocketFile
      interpret = fmap (either ClusterStartupFailure id) . runExceptT

stopClusterHandler :: StopClusterRequest -> AppM StopClusterResponse
stopClusterHandler StopClusterRequest = do
  statusMVar <- asks status
  maybeClusterStatus <- liftIO $ tryTakeMVar statusMVar
  case maybeClusterStatus of
    Nothing -> pure $ StopClusterFailure "Cluster is not running"
    Just statusTVar -> do
      liftIO $ stopCluster statusTVar
      pure StopClusterSuccess

{-# LANGUAGE ExistentialQuantification  #-}

module Plutip.Cluster (
  withCluster,
  startCluster,
  stopCluster,
  withFundedCluster,
  startFundedCluster,
  StopClusterRef,
  dieOnError,
  Lovelace(Lovelace)
) where

import Cardano.Api (ChainTip (ChainTip), SlotNo (SlotNo), Lovelace)
import Cardano.Api qualified as CAPI
import Cardano.BM.Data.Severity qualified as Severity
import Cardano.BM.Data.Tracer (HasPrivacyAnnotation, HasSeverityAnnotation (getSeverityAnnotation))
import Cardano.Launcher (ProcessHasExited (ProcessHasExited))
import Cardano.Startup (installSignalHandlers, setDefaultFilePermissions, withUtf8Encoding)
import Cardano.Wallet.Logging (stdoutTextTracer, trMessageText)
import Control.Monad (unless, void, when, zipWithM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Retry (constantDelay, limitRetries, logRetries, recoverAll, recovering)
import Control.Tracer (Tracer, contramap, traceWith)
import Data.ByteString.Char8 qualified as B
import Data.Foldable (for_)
import Data.Kind (Type)
import Data.Maybe (catMaybes, fromMaybe, isJust)
import Data.Text (Text)
import Data.Text.Class (ToText (toText))
import GHC.IO.Handle (Handle, hDuplicate, hDuplicateTo, hFlush)
import Paths_plutip (getDataFileName)
import System.Directory (
  canonicalizePath,
  createDirectoryIfMissing,
  doesPathExist,
  findExecutable,
  removeDirectoryRecursive,
 )
import System.Environment (setEnv)
import System.Exit (die)
import System.FilePath ((</>))
import System.IO (IOMode (WriteMode), hClose, openFile, stderr, stdout)
import Plutip.Config (
  PlutipConfig (
    clusterDataDir,
    clusterWorkingDir,
    extraConfig
  ),
  WorkingDirectory (Fixed, Temporary),
 )
import Plutip.Launch.Cluster (
  ClusterLog,
  RunningNode,
  testMinSeverityFromEnv,
  TempDirLog, LogOutput (LogToFile), withSystemTempDir, withLoggingNamed
 )
import Plutip.Types (
  ClusterEnv (
    ClusterEnv,
    networkId,
    plutipConf,
    runningNode,
    supportDir,
    tracer
  ),
 )
import Plutip.Tools.CardanoApi qualified as Tools
import UnliftIO.Concurrent (forkFinally, myThreadId, throwTo)
import UnliftIO.Exception (bracket, finally, throwString)
import UnliftIO.STM (TVar, atomically, newTVarIO, readTVar, retrySTM, writeTVar)

import Plutip.Launch.Extra.Utils (localClusterConfigWithExtraConf)
import Cardano.BM.Data.LogItem (LoggerName)
import Plutip.Keys (KeyPair, genKeyPair, cardanoMainnetAddress, saveKeyPair)
import Data.Traversable (for)
import Plutip.DistributeFunds (fundKey)
import Plutip.Tools.CardanoApi (awaitUtxosNumber)
import Plutip.BotPlutusInterface.Setup (keysDir)
import qualified Plutip.Launch.Cluster as Launch

-- | Start a cluster, generate keys, fund their (enterprise) addresses and save the keys inside the cluster working directory.
-- Second argument is a list of desired Ada amounts per utxo per key. 
-- Addresses are already funded when action gets executed.
withFundedCluster :: PlutipConfig -> [[Lovelace]] -> (ClusterEnv -> [KeyPair] -> IO a) -> IO a
withFundedCluster conf distribution action = withCluster conf $ \cenv -> do
  keys <- for distribution (const genKeyPair)
  zipWithM_ (fundKey cenv) keys distribution
  for_ keys (fmap dieOnError . saveKeyPair (keysDir cenv ))
  awaitUtxosNumber cenv
    (cardanoMainnetAddress <$> keys)
    (length $ concat distribution)
    >>= dieOnError
  action cenv keys

-- Like withFundedCluster but keeps cluster running till stopCluster gets called.
startFundedCluster ::
  forall (a :: Type).
  PlutipConfig ->
  [[Lovelace]] ->
  (ClusterEnv -> [KeyPair] -> IO a) ->
  IO (StopClusterRef, a)
startFundedCluster conf distribution action = 
  keepRunning (withFundedCluster conf distribution . curry) (uncurry action)

-- | Like withCluster but keeps cluster running till stopCluster gets called.
startCluster ::
  forall (a :: Type).
  PlutipConfig ->
  (ClusterEnv -> IO a) ->
  IO (StopClusterRef, a)
startCluster conf = keepRunning (withCluster conf)

--- | Send a shutdown signal to the cluster and wait for it
stopCluster :: StopClusterRef -> IO ()
stopCluster (StopClusterRef status) = do
  atomically $ writeTVar status ClusterClosing
  atomically $ readTVar status >>= \case ClusterClosed -> pure (); _ -> retrySTM

{-  
  Launch a local cluster and execute an action.
  A directory gets created containing all cluster related files: keys, logs, configs, database.
  
  Examples:
   `plutus-apps` local cluster: https://github.com/input-output-hk/plutus-apps/blob/75a581c6eb98d36192ce3d3f86ea60a04bc4a52a/plutus-pab/src/Plutus/PAB/LocalCluster/Run.hs
   `cardano-wallet` local cluster: https://github.com/input-output-hk/cardano-wallet/blob/99b13e50f092ffca803fd38b9e435c24dae05c91/lib/shelley/exe/local-cluster.hs
-}
withCluster :: PlutipConfig -> (ClusterEnv -> IO a) -> IO a
withCluster conf action =  do
  withEnvironmentSetup conf $ \dir clusterLogs nodeConfigLogHdl -> do
    withLoggingNamed ("cluster" :: LoggerName) clusterLogs $ \(_, (_, trCluster)) -> do
      let tr' = contramap MsgCluster $ trMessageText trCluster
      clusterCfg <- localClusterConfigWithExtraConf (extraConfig conf)
      withRedirectedStdoutHdl nodeConfigLogHdl $ \restoreStdout -> -- used to mask messy node configuration log
        retryClusterFailedStartup $
          Launch.withCluster tr' dir clusterCfg mempty $ \rn -> do
            restoreStdout $ runAction rn dir trCluster action
  where
    runAction rn dir trCluster userAction = do
      let tracer' = trMessageText trCluster
      waitForRelayNode tracer' rn
      let cEnv = ClusterEnv
            { runningNode = rn
            , networkId = CAPI.Mainnet
            , supportDir = dir
            , tracer = trCluster -- TODO: do we really need it?
            , plutipConf = conf
            }

      userAction cEnv -- executing user action on cluster

    -- Meant to retry startup at the (rare) case of cluster starting on a port that is no longer free
    retryClusterFailedStartup =
      let msg err = B.pack $ "Retrying cluster startup due to: " <> show err <> "\n"
          shouldRetry =
            pure . \case
              ProcessHasExited _ _ -> True
              _ -> False
       in recovering
            (limitRetries 5)
            [logRetries shouldRetry (\_ y _ -> B.hPutStr stderr $ msg y)]
            . const

dieOnError :: Show e => Either e a -> IO a
dieOnError = either (die . show) pure

data StopClusterRef = forall a . StopClusterRef (TVar (ClusterStatus a))

-- Semaphore mechanism to change withCluster-style function to a one that keeps running after the action succeeds.
-- Previously `startCluster`.
keepRunning ::
  ((env -> IO ()) -> IO ())
  -> (env -> IO b) -> IO (StopClusterRef, b)
keepRunning withEnv action = do
  status <- newTVarIO ClusterStarting
  tid <- myThreadId
  void $
    forkFinally
      ( withEnv $ \env -> do
          b <- action env
          atomically $ writeTVar status (ClusterStarted b)
          atomically $ readTVar status >>= \case ClusterClosing -> pure (); _ -> retrySTM
      )
      ( \result -> do
          atomically (writeTVar status ClusterClosed)
          either (throwTo tid) pure result
      )

  setupRes <- atomically $ readTVar status >>= \case ClusterStarted v -> pure v; _ -> retrySTM
  pure (StopClusterRef status, setupRes)

-- Redirect stdout to a provided handle providing mask to temporarily revert back to initial stdout.
withRedirectedStdoutHdl :: Handle -> ((forall b. IO b -> IO b) -> IO a) -> IO a
withRedirectedStdoutHdl hdl action = do
  old_stdout <- hDuplicate stdout
  swapStdout hdl (action $ swapStdout old_stdout)
  where
    swapStdout tmphdl io = do
      hFlush stdout
      old <- hDuplicate stdout
      hDuplicateTo tmphdl stdout
      io `finally` hDuplicateTo old stdout

withDirectory ::
  forall (m :: Type -> Type) (a :: Type).
  MonadUnliftIO m =>
  PlutipConfig ->
  Tracer m TempDirLog ->
  String ->
  (FilePath -> m a) ->
  m a
withDirectory conf tr pathName action =
  case clusterWorkingDir conf of
    Temporary -> withSystemTempDir tr pathName action
    Fixed path shouldKeep -> do
      canonPath <- liftIO $ canonicalizePath path
      liftIO $ doesPathExist canonPath >>= (`when` removeDirectoryRecursive canonPath)
      liftIO $ createDirectoryIfMissing False canonPath
      res <- action canonPath
      unless shouldKeep $ liftIO $ removeDirectoryRecursive canonPath
      return res

-- Do all the program setup required for running the local cluster, create a
-- temporary directory, log output configurations, node_configuration.log handle, and pass these to the given
-- main action.
withEnvironmentSetup ::
  forall (a :: Type).
  PlutipConfig ->
  (FilePath -> [LogOutput] -> Handle -> IO a) ->
  IO a
withEnvironmentSetup conf action = do
  -- current setup requires `cardano-node` and `cardano-cli` as external processes
  checkProcessesAvailable ["cardano-node", "cardano-cli"]

  setClusterDataDir

  -- Handle SIGTERM properly
  installSignalHandlers (putStrLn "Terminated")

  -- Ensure key files have correct permissions for cardano-cli
  setDefaultFilePermissions

  -- Set UTF-8, regardless of user locale
  withUtf8Encoding $
    -- This temporary directory will contain logs, and all other data
    -- produced by the local test cluster.
    withDirectory conf stdoutTextTracer "test-cluster" $ \dir -> do
      let logOutputs name minSev =
            -- cluster logs to file only
            [LogToFile (dir </> name) (min minSev Severity.Info)]

      clusterLogs <- logOutputs "cluster.log" <$> testMinSeverityFromEnv

      bracket
        (openFile (dir </> "node_configuration.log") WriteMode)
        hClose
        (action dir clusterLogs)
  where
    setClusterDataDir = do
      defaultClusterDataDir <- getDataFileName "cluster-data"
      setEnv "SHELLEY_TEST_DATA" $
        fromMaybe defaultClusterDataDir (clusterDataDir conf)

checkProcessesAvailable :: [String] -> IO ()
checkProcessesAvailable requiredProcesses = do
  results <- mapM findExecutable requiredProcesses
  unless (isJust `all` results) $
    die $
      "This processes should be available in the environment:\n "
        <> show requiredProcesses
        <> "\n but only these were found:\n "
        <> show (catMaybes results)

waitForRelayNode :: Tracer IO TestsLog -> RunningNode -> IO ()
waitForRelayNode trCluster rn =
  liftIO $ do
    recoverAll policy wait
  where
    -- TODO: move this to config
    policy = constantDelay 1_000_000 <> limitRetries 60
    getTip = trace >> Tools.queryTip rn
    trace = traceWith trCluster WaitingRelayNode
    wait _ = do
      tip <- getTip
      case tip of
        ChainTip (SlotNo _) _ _ -> pure ()
        a -> throwString $ "Timeout waiting for node to start. Last 'tip' response:\n" <> show a
      pure ()

data ClusterStatus (a :: Type)
  = ClusterStarting
  | ClusterStarted a
  | ClusterClosing
  | ClusterClosed

-- Logging

data TestsLog
  = MsgBaseUrl Text Text Text -- wallet url, ekg url, prometheus url
  | MsgSettingUpFaucet
  | MsgCluster ClusterLog
  | WaitingRelayNode
  deriving stock (Show)

instance ToText TestsLog where
  toText = \case
    MsgBaseUrl walletUrl ekgUrl prometheusUrl ->
      mconcat
        [ "Wallet url: "
        , walletUrl
        , ", EKG url: "
        , ekgUrl
        , ", Prometheus url:"
        , prometheusUrl
        ]
    MsgSettingUpFaucet -> "Setting up faucet..."
    MsgCluster msg -> toText msg
    WaitingRelayNode -> "Waiting for relay node up and running"

instance HasPrivacyAnnotation TestsLog

instance HasSeverityAnnotation TestsLog where
  getSeverityAnnotation = \case
    MsgSettingUpFaucet -> Severity.Notice
    MsgBaseUrl {} -> Severity.Notice
    MsgCluster msg -> getSeverityAnnotation msg
    WaitingRelayNode -> Severity.Notice

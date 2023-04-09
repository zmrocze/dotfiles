module Test.Plutip.Internal.LocalCluster (
  startCluster,
  stopCluster,
  withLocalCluster,
  ClusterStatus (
    ClusterStarting,
    ClusterStarted,
    ClusterClosing,
    ClusterClosed
  ),
withFundedLocalCluster) where

import Cardano.Api (ChainTip (ChainTip), SlotNo (SlotNo))
import Cardano.Api qualified as CAPI
import Cardano.BM.Data.Severity qualified as Severity
import Cardano.BM.Data.Tracer (HasPrivacyAnnotation, HasSeverityAnnotation (getSeverityAnnotation))
-- import Cardano.CLI (LogOutput (LogToFile), withLoggingNamed)
import Cardano.Launcher (ProcessHasExited (ProcessHasExited))
import Cardano.Startup (installSignalHandlers, setDefaultFilePermissions, withUtf8Encoding)
import Cardano.Wallet.Logging (stdoutTextTracer, trMessageText)
-- import Cardano.Wallet.Shelley.Launch (TempDirLog, withSystemTempDir)
-- import Cardano.Wallet.Shelley.Network (TempDirLog, withSystemTempDir)
-- import Cardano.Wallet.Shelley.Launch.Cluster (ClusterLog, localClusterConfigFromEnv, testMinSeverityFromEnv, walletMinSeverityFromEnv, withCluster)
-- import Cardano.Wallet.Shelley.Launch (TempDirLog, withSystemTempDir)
-- import Cardano.Wallet.Shelley.Network (TempDirLog, withSystemTempDir)
-- import Cardano.Wallet.Shelley.Launch.Cluster (ClusterLog, localClusterConfigFromEnv, testMinSeverityFromEnv, walletMinSeverityFromEnv, withCluster)
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
import GHC.Stack.Types (HasCallStack)
import Paths_plutip (getDataFileName)
import System.Directory (
  canonicalizePath,
  copyFile,
  createDirectoryIfMissing,
  doesPathExist,
  findExecutable,
  removeDirectoryRecursive,
 )
import System.Environment (setEnv)
import System.Exit (die)
import System.FilePath ((</>))
import System.IO (IOMode (WriteMode), hClose, openFile, stderr, stdout)
import Test.Plutip.Config (
  PlutipConfig (
    clusterDataDir,
    clusterWorkingDir,
    extraConfig
  ),
  WorkingDirectory (Fixed, Temporary),
 )
import Test.Plutip.Internal.Cluster (
  ClusterLog,
  RunningNode,
  testMinSeverityFromEnv,
  withCluster, TempDirLog, LogOutput (LogToFile), withSystemTempDir, withLoggingNamed
 )
import Test.Plutip.Internal.Types (
  ClusterEnv (
    ClusterEnv,
    networkId,
    plutipConf,
    runningNode,
    supportDir,
    tracer
  ),
 )
import Test.Plutip.Tools.CardanoApi qualified as Tools
import UnliftIO.Concurrent (forkFinally, myThreadId, throwTo)
import UnliftIO.Exception (bracket, finally, throwString)
import UnliftIO.STM (TVar, atomically, newTVarIO, readTVar, retrySTM, writeTVar)

import Test.Plutip.Internal.Cluster.Extra.Utils (localClusterConfigWithExtraConf)
import Cardano.BM.Data.LogItem (LoggerName)
import Numeric.Positive (Positive)
import Test.Plutip.Internal.Keys (KeyPair, genKeyPair, cardanoMainnetAddress, saveKeyPair)
import Data.Traversable (for)
import Test.Plutip.Internal.DistributeFunds (fundKey)
import Test.Plutip.Tools.CardanoApi (awaitUtxosNumber)
import Test.Plutip.Internal.BotPlutusInterface.Setup (keysDir)

-- | Start a cluster, generate keys, fund their (enterprise) addresses and save the keys inside the cluster working directory.
-- Second argument is a list of desired Ada amounts per utxo per key. 
-- Addresses are already funded when action gets executed.
withFundedLocalCluster :: PlutipConfig -> [[Positive]] -> (ClusterEnv -> [KeyPair] -> IO a) -> IO a
withFundedLocalCluster conf distribution action = withLocalCluster conf $ \cenv -> do
  keys <- for distribution (const genKeyPair)
  zipWithM_ (fundKey cenv) keys distribution
  for_ keys (fmap dieOnError . saveKeyPair (keysDir cenv ))
  awaitUtxosNumber cenv
    (cardanoMainnetAddress <$> keys)
    (length $ concat distribution)
    >>= dieOnError
  action cenv keys

dieOnError :: Show e => Either e a -> IO a
dieOnError = either (die . show) pure

keepRunning :: ((a -> IO b) -> IO b) -> ((a -> IO b) -> IO (TVar (ClusterStatus a), b))
keepRunning

-- | Like withCluster but keeps cluster running till stopCluster gets called.
startCluster ::
  forall (a :: Type).
  PlutipConfig ->
  (ClusterEnv -> IO a) ->
  IO (TVar (ClusterStatus a), a)
startCluster conf onClusterStart = do
  status <- newTVarIO ClusterStarting
  tid <- myThreadId
  void $
    forkFinally
      ( withLocalCluster conf $ \clusterEnv -> do
          res <- onClusterStart clusterEnv
          atomically $ writeTVar status (ClusterStarted res)
          atomically $ readTVar status >>= \case ClusterClosing -> pure (); _ -> retrySTM
      )
      ( \result -> do
          atomically (writeTVar status ClusterClosed)
          either (throwTo tid) pure result
      )

  setupRes <- atomically $ readTVar status >>= \case ClusterStarted v -> pure v; _ -> retrySTM
  pure (status, setupRes)

--- | Send a shutdown signal to the cluster and wait for it
stopCluster :: TVar (ClusterStatus a) -> IO ()
stopCluster status = do
  atomically $ writeTVar status ClusterClosing
  atomically $ readTVar status >>= \case ClusterClosed -> pure (); _ -> retrySTM

withLocalCluster :: PlutipConfig -> (ClusterEnv -> IO a) -> IO a
withLocalCluster conf action =  do
  withEnvironmentSetup conf $ \dir clusterLogs nodeConfigLogHdl -> do
    withLoggingNamed ("cluster" :: LoggerName) clusterLogs $ \(_, (_, trCluster)) -> do
      let tr' = contramap MsgCluster $ trMessageText trCluster
      clusterCfg <- localClusterConfigWithExtraConf (extraConfig conf)
      withRedirectedStdoutHdl nodeConfigLogHdl $ \restoreStdout -> -- used to mask messy node configuration log
        retryClusterFailedStartup $
          withCluster tr' dir clusterCfg mempty $ \rn -> do
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

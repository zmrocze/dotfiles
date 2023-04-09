module Main (
  main
) where

import Cardano.Api qualified as CAPI
import Cardano.BM.Data.Severity qualified as Severity
import Cardano.BM.Data.Tracer (HasPrivacyAnnotation, HasSeverityAnnotation (getSeverityAnnotation))
import Cardano.BM.Configuration.Model qualified as CM
import Cardano.CLI (LogOutput (LogToFile), withLoggingNamed)
import Cardano.Launcher.Node (nodeSocketFile)
import Cardano.Startup (installSignalHandlers, setDefaultFilePermissions, withUtf8Encoding)
import Cardano.Wallet.Logging (stdoutTextTracer, trMessageText)
import Cardano.Wallet.Shelley.Launch (TempDirLog, withSystemTempDir)
import Cardano.Wallet.Shelley.Launch.Cluster (ClusterLog, localClusterConfigFromEnv, testMinSeverityFromEnv, walletMinSeverityFromEnv, withCluster)
import Control.Concurrent.Async (async)
import Control.Monad (unless, void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (ReaderT (runReaderT))
import Control.Retry (constantDelay, limitRetries, recoverAll)
import Control.Tracer (Tracer, contramap, traceWith)
import Data.Foldable (for_)
import Data.Kind (Type)
import Data.Maybe (catMaybes, fromMaybe, isJust)
import Data.Text (Text, pack)
import Data.Text.Class (ToText (toText))
import GHC.IO.Handle (Handle, hDuplicate, hDuplicateTo, hFlush)
import GHC.Stack.Types (HasCallStack)
import Paths_plutip (getDataFileName)
import Plutus.ChainIndex.App qualified as ChainIndex
import Plutus.ChainIndex.Config (ChainIndexConfig (cicNetworkId, cicPort), cicDbPath, cicSocketPath)
import Plutus.ChainIndex.Config qualified as ChainIndex
import Plutus.ChainIndex.Logging (defaultConfig)
import Servant.Client (BaseUrl (BaseUrl), Scheme (Http))
import System.Directory (canonicalizePath, copyFile, createDirectoryIfMissing, doesPathExist, findExecutable, removeDirectoryRecursive)
import System.Environment (setEnv)
import System.Exit (die)
import System.FilePath ((</>))
import System.IO (IOMode (WriteMode), hClose, openFile, stdout)
import Test.Plutip.Config (
  PlutipConfig (
    chainIndexPort,
    clusterDataDir,
    clusterWorkingDir,
    relayNodeLogs
  ),
  WorkingDirectory (Fixed, Temporary),
 )
import Test.Plutip.Internal.BotPlutusInterface.Setup qualified as BotSetup
import Test.Plutip.Internal.Types (
  ClusterEnv (
    ClusterEnv,
    chainIndexUrl,
    networkId,
    plutipConf,
    runningNode,
    supportDir,
    tracer
  ),
  RunningNode (RunningNode),
 )
import Test.Plutip.Tools.CardanoApi qualified as Tools
import Text.Printf (printf)
import UnliftIO.Concurrent (forkFinally, myThreadId, throwTo)
import UnliftIO.Exception (bracket, catchIO, finally)
import UnliftIO.STM (TVar, atomically, newTVarIO, readTVar, retrySTM, writeTVar)

launchChainIndex :: PlutipConfig -> RunningNode -> FilePath -> IO Int
launchChainIndex conf (RunningNode sp _block0 (_gp, _vData) _) dir = do
  config <- defaultConfig
  CM.setMinSeverity config Severity.Notice
  let dbPath = dir </> "chain-index.db"
      chainIndexConfig =
        ChainIndex.defaultConfig
          { cicSocketPath = nodeSocketFile sp
          , cicDbPath = dbPath
          , cicNetworkId = CAPI.Mainnet
          , cicPort =
              maybe
                (cicPort ChainIndex.defaultConfig)
                fromEnum
                (chainIndexPort conf)
          }
  void . async $ void $ ChainIndex.runMainWithLog (const $ return ()) config chainIndexConfig
  return $ cicPort chainIndexConfig
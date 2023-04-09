module Test.Plutip.Internal.Types (
  ClusterEnv (..),
  RunningNode (..),
  nodeSocket
) where

import Cardano.Api (NetworkId)
import Cardano.BM.Tracing (Trace)
import Cardano.Launcher.Node (CardanoNodeConn)

-- import Cardano.Wallet.Shelley.Launch.Cluster (RunningNode (RunningNode))
import Data.Text (Text)
import Test.Plutip.Config (PlutipConfig)
import Test.Plutip.Internal.Cluster (RunningNode (RunningNode))

-- | Environment for actions that use local cluster
data ClusterEnv = ClusterEnv
  { runningNode :: RunningNode
  -- , chainIndexUrl :: !(Maybe BaseUrl)
  , networkId :: !NetworkId
  , -- | this directory atm used to store all node related files,
    -- files created by `cardano-cli`, `chain-index` and `bot-plutus-interface`
    supportDir :: FilePath
  , tracer :: Trace IO Text -- not really used anywhere now
  , plutipConf :: !PlutipConfig
  }

-- | Helper function to get socket path from
nodeSocket :: ClusterEnv -> CardanoNodeConn
nodeSocket (ClusterEnv (RunningNode sp _ _ _) _ _ _ _) = sp

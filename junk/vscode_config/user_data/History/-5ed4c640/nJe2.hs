module Plutip.Types (
  ClusterEnv (..),
  RunningNode (..),
  nodeSocket
) where

import Cardano.Api (NetworkId)
import Cardano.BM.Tracing (Trace)
import Cardano.Launcher.Node (CardanoNodeConn)

import Data.Text (Text)
import Plutip.Config (PlutipConfig)
import Plutip.Launch.Cluster (RunningNode (RunningNode))

-- | Environment for actions that use local cluster
data ClusterEnv = ClusterEnv
  { runningNode :: RunningNode
  -- , chainIndexUrl :: !(Maybe BaseUrl)
  , networkId :: !NetworkId
  , -- | this directory atm used to store all node related files,
    -- files created by `cardano-cli`, `chain-index` and `bot-plutus-interface`
    supportDir :: FilePath
  , plutipConf :: !PlutipConfig
  }

-- | Helper function to get socket path from
nodeSocket :: ClusterEnv -> CardanoNodeConn
nodeSocket (ClusterEnv (RunningNode sp _ _ _) _ _ _ _) = sp

module Plutip.Cluster.Extra.Utils (
  localClusterConfigWithExtraConf,
) where

import Plutip.Cluster (LocalClusterConfig (LocalClusterConfig), clusterEraFromEnv, clusterEraToString, logFileConfigFromEnv)
import Plutip.Cluster.Extra.Types (ExtraConfig)
import Plutip.Cluster.PoolConfigs (defaultPoolConfigs)

localClusterConfigWithExtraConf :: ExtraConfig -> IO LocalClusterConfig
localClusterConfigWithExtraConf ec = do
  era <- clusterEraFromEnv
  logConf <- logFileConfigFromEnv (Just $ clusterEraToString era)
  pure $ LocalClusterConfig defaultPoolConfigs era logConf ec

module PlutipCluster.Extra.Utils (
  localClusterConfigWithExtraConf,
) where

import PlutipCluster (LocalClusterConfig (LocalClusterConfig), clusterEraFromEnv, clusterEraToString, logFileConfigFromEnv)
import PlutipCluster.Extra.Types (ExtraConfig)
import PlutipCluster.PoolConfigs (defaultPoolConfigs)

localClusterConfigWithExtraConf :: ExtraConfig -> IO LocalClusterConfig
localClusterConfigWithExtraConf ec = do
  era <- clusterEraFromEnv
  logConf <- logFileConfigFromEnv (Just $ clusterEraToString era)
  pure $ LocalClusterConfig defaultPoolConfigs era logConf ec

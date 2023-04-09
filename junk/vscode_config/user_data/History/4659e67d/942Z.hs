module Main (
  main
) where

launchChainIndex :: PlutipConfig -> RunningNode -> FilePath -> IO Int
launchChainIndex conf (RunningNode  _block0 (_gp, _vData) _) dir = do
  config <- defaultConfig
  CM.setMinSeverity config Severity.Notice
  let dbPath = dir </> "chain-index.db"
      chainIndexConfig =
        ChainIndex.defaultConfig
          { cicSocketPath = "/home/zmrocze/mlabs/plutip/master/my-local-cluster/pool-1/node.socket"
          , cicDbPath = dbPath
          , cicNetworkId = CAPI.Mainnet
          , cicPort =
              maybe
                (cicPort ChainIndex.defaultConfig)
                fromEnum
                (chainIndexPort conf)
          }
  ChainIndex.runMainWithLog (const $ return ()) config chainIndexConfig
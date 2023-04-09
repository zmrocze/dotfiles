module Main (
  main
) where

launchChainIndex :: FilePath -> IO Int
launchChainIndex dir = do
  config <- defaultConfig
  CM.setMinSeverity config Severity.Notice
  let dbPath = dir </> "chain-index.db"
      chainIndexConfig =
        ChainIndex.defaultConfig
          { cicSocketPath = "/home/zmrocze/mlabs/plutip/master/my-local-cluster/pool-1/node.socket"
          , cicDbPath = dbPath
          , cicNetworkId = CAPI.Mainnet
        --   , cicPort =
        --       maybe
        --         (cicPort ChainIndex.defaultConfig)
        --         fromEnum
        --         (chainIndexPort conf)
          }
  ChainIndex.runMainWithLog (const $ return ()) config chainIndexConfig

main = launchChainIndex 
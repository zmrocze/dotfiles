{-# LANGUAGE ImportQualifiedPost  #-}

module Main (
  main
) where

import Test.Plutip.PlutipBug (mymain)
-- import Cardano.Api as CAPI
-- import Cardano.BM.Data.Severity qualified as Severity

-- import Cardano.BM.Configuration.Model qualified as CM

-- import Paths_plutip (getDataFileName)
-- import Plutus.ChainIndex.App qualified as ChainIndex
-- import Plutus.ChainIndex.Config (ChainIndexConfig (cicNetworkId, cicPort), cicDbPath, cicSocketPath)
-- import Plutus.ChainIndex.Config qualified as ChainIndex
-- import Plutus.ChainIndex.Logging (defaultConfig)

-- import Test.Plutip.Config (
--   PlutipConfig (
--     chainIndexPort,
--     clusterDataDir,
--     clusterWorkingDir,
--     relayNodeLogs
--   ),
--   WorkingDirectory (Fixed, Temporary),
--  )
-- import Test.Plutip.Internal.BotPlutusInterface.Setup qualified as BotSetup
-- import Test.Plutip.Internal.Types (
--   ClusterEnv (
--     ClusterEnv,
--     chainIndexUrl,
--     networkId,
--     plutipConf,
--     runningNode,
--     supportDir,
--     tracer
--   ),
--   RunningNode (RunningNode),
--  )


-- launchChainIndex :: IO ()
-- launchChainIndex = do
--   config <- defaultConfig
--   CM.setMinSeverity config Severity.Notice
--   let dbPath = "/home/zmrocze/mlabs/plutip/master/my-local-cluster/chain-index.db"
--       chainIndexConfig =
--         ChainIndex.defaultConfig
--           { cicSocketPath = "/home/zmrocze/mlabs/plutip/master/my-local-cluster/pool-1/node.socket"
--           , cicDbPath = dbPath
--           , cicNetworkId = CAPI.Mainnet
--         --   , cicPort =
--         --       maybe
--         --         (cicPort ChainIndex.defaultConfig)
--         --         fromEnum
--         --         (chainIndexPort conf)
--           }
--   ChainIndex.runMainWithLog (const $ return ()) config chainIndexConfig

-- main1 = do 
--   cl <- startCluster conf setup 
--   stopCluster . fst
--   forever $ threadDelay 1000
        
--   where
--     setup :: ReaderT ClusterEnv IO (ClusterEnv, [NonEmpty BpiWallet])
--     setup = do
--       env <- ask

--       wallets <-
--         traverse
--           (traverse addSomeWallet . fmap twInitDistribuition . unTestWallets . fst)
--           testCases
--       waitSeconds 2 -- wait for transactions to submit
--       pure (env, wallets)


main = do 
    print "hey\n"
    mymain 
{-# LANGUAGE ImportQualifiedPost  #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Test.Plutip.PlutipBug (
  mymain
, launchChainIndex) where


import Cardano.Api as CAPI
import Cardano.BM.Data.Severity qualified as Severity

import Cardano.BM.Configuration.Model qualified as CM

import Paths_plutip (getDataFileName)
import Plutus.ChainIndex.App qualified as ChainIndex
import Plutus.ChainIndex.Config (ChainIndexConfig (cicNetworkId, cicPort), cicDbPath, cicSocketPath)
import Plutus.ChainIndex.Config qualified as ChainIndex
import Plutus.ChainIndex.Logging (defaultConfig)

import Test.Plutip.LocalCluster (startCluster, stopCluster, waitSeconds)
import Control.Monad.Reader (forever, MonadReader (ask))
-- import BotPlutusInterface.Effects (threadDelay)
import Data.Default (Default(def))
import Control.Concurrent (threadDelay)


launchChainIndex :: IO ()
launchChainIndex = do
  config <- defaultConfig
  CM.setMinSeverity config Severity.Notice
  let dbPath = "/home/zmrocze/mlabs/plutip/master/my-local-cluster/chain-index.db"
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

main1 :: IO ()
main1 = do 
  cl <- startCluster def setup 
  stopCluster $ fst cl
  forever $ threadDelay 1000
        
  where
    -- setup :: ReaderT ClusterEnv IO (ClusterEnv, [NonEmpty BpiWallet])
    setup = do
      env <- ask

      wallets <- return ()
      waitSeconds 2 -- wait for transactions to submit
      pure (env, wallets)


mymain :: IO ()
mymain = main1
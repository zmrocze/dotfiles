module Plutip.BotPlutusInterface.Setup (
  keysDir,
  directoryIsSet,
  pParamsFile,
) where

import Plutip.Types (ClusterEnv (supportDir))
import System.Directory (doesDirectoryExist)
import System.FilePath ((</>))

-- TODO: remove bot-plutus-interface
keysDir' :: FilePath
keysDir' = "signing-keys"

-- | Get directory for `.skey`'s of crated wallets for current cluster environment
keysDir :: ClusterEnv -> FilePath
keysDir cEnv = supportDir cEnv </> keysDir'

-- | Check if required by bot interface directories exist
directoryIsSet :: ClusterEnv -> IO Bool
directoryIsSet cEnv = doesDirectoryExist $ keysDir cEnv

-- | Protocol parameters file required for bot interface
pParamsFile :: ClusterEnv -> FilePath
pParamsFile cEnv = supportDir cEnv </> workDir' </> "pparams.json"

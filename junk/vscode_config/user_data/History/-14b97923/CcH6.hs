module Test.Plutip.Internal.BotPlutusInterface.Setup (
  keysDir,
  directoryIsSet,
  pParamsFile
) where

import System.Directory (doesDirectoryExist)
import System.FilePath ((</>))
import Test.Plutip.Internal.Types (ClusterEnv (supportDir))

workDir' :: FilePath
workDir' = "bot-plutus-interface"

keysDir' :: FilePath
keysDir' = workDir' </> "signing-keys"

-- | Get directory for `.skey`'s of crated wallets for current cluster environment
keysDir :: ClusterEnv -> FilePath
keysDir cEnv = supportDir cEnv </> keysDir'

-- | Check if required by bot interface directories exist
directoryIsSet :: ClusterEnv -> IO Bool
directoryIsSet cEnv = doesDirectoryExist $ keysDir cEnv

-- | Protocol parameters file required for bot interface
pParamsFile :: ClusterEnv -> FilePath
pParamsFile cEnv = supportDir cEnv </> workDir' </> "pparams.json"

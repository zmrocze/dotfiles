module Plutip.BotPlutusInterface.Setup (
  keysDir,
  directoryIsSet,
  pParamsFile,
) where

import Plutip.Types (ClusterEnv (supportDir))
import System.Directory (doesDirectoryExist)
import System.FilePath ((</>))

keysDir' :: FilePath
keysDir' = "signing-keys"

-- | Get directory for `.skey`'s of crated wallets for current cluster environment
keysDir :: ClusterEnv -> FilePath
keysDir cEnv = supportDir cEnv </> keysDir'


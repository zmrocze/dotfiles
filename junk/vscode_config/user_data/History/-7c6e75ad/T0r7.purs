-- | This module, when bundled, executes the default contract in the browser or
-- | the Node.
module MLabsPlutusTemplate.Main (main) where

import Contract.Prelude

import Contract.Address (getWalletAddresses)
import Contract.Config as Contract.Config
import Contract.Monad as Contract.Monad
import MLabsPlutusTemplate.Scripts (always_succeeds)

-- main :: Effect Unit
-- main = Contract.Monad.launchAff_
--   $ void
--   $ Contract.Monad.runContract Contract.Config.testnetNamiConfig
--   $ getWalletAddresses

main :: forall t1. MonadEffect t1 => t1 Unit
main = log always_succeeds
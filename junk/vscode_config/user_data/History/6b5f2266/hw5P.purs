-- | This module, when bundled, executes the default contract in the browser or
-- | the Node.
module Scaffold.Main (main) where

import Contract.Prelude

import Contract.Config (PrivatePaymentKeySource(..), WalletSpec(..))
import Contract.Config as Contract.Config
import Contract.Monad as Contract.Monad
import Ctl.Internal.Serialization.Types (PrivateKey)
import Scaffold as Scaffold

main :: Effect Unit
main = Contract.Monad.launchAff_
  $ void
  $ Contract.Monad.runContract (Contract.Config.testnetNamiConfig
          { walletSpec = Just $ UseKeys
              (PrivatePaymentKeyValue $ wrap (PrivateKey 5))
              Nothing
          , customLogger = Just printLog
          , ctlServerConfig = Nothing
          })
  $ Scaffold.contract

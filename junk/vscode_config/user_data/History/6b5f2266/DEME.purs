-- | This module, when bundled, executes the default contract in the browser or
-- | the Node.
module Scaffold.Main (main) where

import Contract.Prelude

import Contract.Config (PrivatePaymentKeySource(..), WalletSpec(..))
import Contract.Config as Contract.Config
import Contract.Monad as Contract.Monad
import Scaffold as Scaffold
import Test.Unit.Console (print)

main :: Effect Unit
main = do 
  print "HelloWrolds "
  print Scaffold.mymain
-- main = Contract.Monad.launchAff_
--   $ void
--   $ Contract.Monad.runContract (Contract.Config.testnetNamiConfig
--           { walletSpec = Just $ UseKeys
--                 (PrivatePaymentKeyValue $ wrap undefined)
--                 Nothing
--           , ctlServerConfig = Nothing
--           })
--   $ Scaffold.contract



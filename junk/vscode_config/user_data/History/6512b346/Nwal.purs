module Test.ClusterBug where

import Contract.Prelude (Effect, Unit, ($))
import Ctl.Internal.Plutip.Server (checkPlutipServer)
import Effect.Aff (launchAff_)
import Test.Ctl.Plutip.Common (config)

randomTimeout :: Effect Unit
randomTimeout = do

main :: Effect Unit
main = launchAff_ $ checkPlutipServer config

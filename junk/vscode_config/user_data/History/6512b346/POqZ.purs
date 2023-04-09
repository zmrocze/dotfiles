module Test.ClusterBug where

import Contract.Prelude (Effect, Unit, ($))
import Ctl.Internal.Plutip.Server (checkPlutipServer)
import Effect.Aff (launchAff_)
import Effect.Random (random)
import Test.Ctl.Plutip.Common (config)

randomTimeout :: Effect Unit
randomTimeout = do
    r <- random
    unit <$ setTimeout 1000 (cb int)
    


main :: Effect Unit
main = launchAff_ $ checkPlutipServer config

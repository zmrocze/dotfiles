module Test.ClusterBug where

import Contract.Prelude (Effect, Unit, bind, pure, unit, ($), (<$))
import Ctl.Internal.Plutip.Server (checkPlutipServer)
import Effect.Aff (launchAff_)
import Effect.Random (random)
import Effect.Timer (setTimeout)
import Test.Ctl.Plutip.Common (config)

randomTimeout :: Effect Unit
randomTimeout = do
    r <- random
    unit <$ setTimeout 1000 (pure unit)



main :: Effect Unit
main = launchAff_ $ checkPlutipServer config

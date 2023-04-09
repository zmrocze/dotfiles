module Test.ClusterBug where

import Contract.Prelude (Effect, Unit, bind, pure, unit, ($), (<$))
import Ctl.Internal.Plutip.Server (checkPlutipServer)
import Effect.Aff (launchAff_)
import Effect.Random (random)
import Effect.Timer (setTimeout)
import Test.Ctl.Plutip.Common (config)

randomTimeout :: Aff Unit
randomTimeout = do
    r <- liftAff random
    



main :: Effect Unit
main = launchAff_ $ checkPlutipServer config

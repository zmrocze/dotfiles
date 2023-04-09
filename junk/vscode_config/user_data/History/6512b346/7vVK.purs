module Test.ClusterBug where

-- import Contract.Prelude (Effect, Unit, bind, pure, unit, ($), (<$))
import Contract.Prelude
import Ctl.Internal.Plutip.Server (checkPlutipServer)
import Data.Time (Millisecond)
import Effect.Aff (delay, launchAff_)
import Effect.Random (random)
import Effect.Timer (setTimeout)
import Test.Ctl.Plutip.Common (config)

randomTimeout :: Aff Unit
randomTimeout = do
    r <- liftAff random
    delay $ Milliseconds (r * 1000)



main :: Effect Unit
main = launchAff_ $ checkPlutipServer config

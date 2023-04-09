module Test.ClusterBug where

-- import Contract.Prelude (Effect, Unit, bind, pure, unit, ($), (<$))

import Contract.Prelude

import Ctl.Internal.Plutip.Server (checkPlutipServer)
import Data.List.Lazy (replicateM)
import Effect.Aff (Milliseconds(..), delay, launchAff_)
import Effect.Random (random)
import Test.Ctl.Plutip.Common (config)
import Test.Unit.Console (print)

randomTimeout :: Aff Unit
randomTimeout = do
    r <- liftEffect random
    delay $ Milliseconds (r * 1000.0)



main :: Effect Unit
main = launchAff_ $ do 
    print "hello"
    replicateM randomTimeout 
    
    print 
    checkPlutipServer config

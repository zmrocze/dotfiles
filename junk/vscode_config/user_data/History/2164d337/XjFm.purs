module Ctl.Examples.ErrorTest where

import Contract.Prelude
import Prelude

import Contract.Monad (throwContractError)
import Control.Monad.Error.Class (throwError, try)
import Data.List ((..))
import Effect.Aff (Error, error, fiberCanceler, launchAff, launchAff_, makeAff, nonCanceler)
import Test.Unit.Console (print)

main :: Effect Unit
main = do 
    print "hello"
    -- launchAff_ $
    --     void $ liftEffect $ try $ (launchAff_ (throwError (error "Error!") :: Aff Unit))

    -- launchAff_ $
    --     void $ liftEffect $ try $ notId $ (throw "Silent Error" :: Effect Unit)

    -- launchAff_ $
    --     void $ liftEffect $ try $ launchAff_ $ (throwError (error "Error") :: Aff Unit)

    -- launchAff_ $
    --     void $ liftEffect $ try $ a unit

    let (signingThatErrors :: Aff Unit) = throwError (error "Error!")

    let succeeds = launchAff_ $ try $ 
            for (1..5) $ \_ -> 
                makeAff $ \k -> map (fiberCanceler) $ 
                    launchAff $ subscribe $ (\x -> rethrow k signingThatErrors)

    let fails = void $ launchAff_ $ try $ for (1..5) $ \_ -> 
            subscribe $ (\x -> launchAff_ signingThatErrors)
    
    succeeds
    -- fails

-- makeAffRethrowing ∷ ∀ (a ∷ Type) (b ∷ Type). Aff a → Aff b

makeAffRethrowing ∷ ∀ (t42 ∷ Type) (t46 ∷ Type). ((Aff t42 → Effect Unit) → Aff t46) → Aff t42
makeAffRethrowing aff = 
    let rethrow k aff' = launchAff_ $ try aff' >>= (k >>> liftEffect)
    in makeAff $ \k -> 
        fiberCanceler <$> launchAff (aff $ rethrow k)
 

rethrow :: forall a . (Either Error a -> Effect Unit) -> Aff a -> Effect Unit
rethrow k aff = launchAff_ $ try aff >>= (k >>> liftEffect)
    

subscribe ∷ (Unit → Effect Unit) → Aff Unit
subscribe cont = void $ liftEffect $ try $ cont unit

a :: Unit -> Effect Unit
a _ = launchAff_ (throwError (error "Bad sign"))

-- ignore = makeAff

notId :: Effect Unit -> Effect Unit
notId = liftEffect >>> launchAff_
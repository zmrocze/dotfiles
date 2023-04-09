-- | This module contains a minimal contract that only prints public key
-- | info to the console.
module Scaffold
  ( contract
  , mymain
  )
  where

import Contract.Prelude

import Contract.Address (ownPaymentPubKeyHash)
import Contract.Log (logInfo, logInfo')
import Contract.Monad (Contract)
import Contract.Scripts (Validator(..))
import Contract.TextEnvelope (decodeTextEnvelope, plutusScriptV1FromEnvelope)
import Control.Monad.Error.Class (liftMaybe)
import Effect.Exception (error)
import Test.Unit.Console (print)

foreign import myscript :: String

contract :: Contract () Unit
contract = do
  logInfo' "Welcome to CTL! Your wallet's payment PubKey hash is:"
  -- logInfo' <<< show =<< ownPaymentPubKeyHash
  -- myContract

mymain = do 
  print (show parseValidator)


-- parseValidator :: Contract () Validator
-- parseValidator = liftMaybe (error "Error decoding myscript") do
--     envelope <- decodeTextEnvelope myscript
--     Validator <$> plutusScriptV1FromEnvelope envelope

parseValidator :: Maybe Validator
parseValidator = do
    envelope <- decodeTextEnvelope myscript
    Validator <$> plutusScriptV1FromEnvelope envelope

-- myContract ∷ Contract () Unit
-- myContract = do
--   validator <- parseValidator
--   logInfo' (show validator)
--   logInfo' "heLOo"
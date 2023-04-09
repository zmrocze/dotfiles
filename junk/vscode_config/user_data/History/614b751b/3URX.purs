-- | This module contains a minimal contract that only prints public key
-- | info to the console.
module Scaffold (contract) where

import Contract.Prelude

import Contract.Address (ownPaymentPubKeyHash)
import Contract.Log (logInfo')
import Contract.Monad (Contract)

contract :: Contract () Unit
contract = do
  logInfo' "Welcome to CTL! Your wallet's payment PubKey hash is:"
  logInfo' <<< show =<< ownPaymentPubKeyHash


foreign import myscript :: String

parseValidator :: Contract () Validator
parseValidator = liftMaybe (error "Error decoding myscript") do
    envelope <- decodeTextEnvelope myscript
    Validator <$> Contract.TextEnvelope.plutusScriptV1FromEnvelope envelope

myContract cfg = runContract_ cfg $ do
  validator <- parseValidator
  
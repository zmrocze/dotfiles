module Test.Plutip.Internal.BotPlutusInterface.Keys (genKeyPair) where

import Cardano.Api (AsType (AsPaymentKey), Key (VerificationKey, getVerificationKey), PaymentKey, SigningKey, generateSigningKey)

data KeyPair = KeyPair
  { sKey :: SigningKey PaymentKey
  , vKey :: VerificationKey PaymentKey
  }
  deriving stock (Show)

genKeyPair :: IO KeyPair
genKeyPair = do
  sKey <- generateSigningKey AsPaymentKey
  return $ KeyPair sKey (getVerificationKey sKey)


module Test.Plutip.Internal.Keys (KeyPair) where 

import Cardano.Api (AsType (AsPaymentKey), Key (VerificationKey, getVerificationKey, verificationKeyHash), PaymentKey, SigningKey, TextEnvelopeDescr, generateSigningKey, writeFileTextEnvelope, AddressAny)
import System.FilePath ((<.>), (</>))
import qualified Cardano.Api as CAPI
import qualified Data.Text as Text

data KeyPair = KeyPair
  { sKey :: SigningKey PaymentKey
  , vKey :: VerificationKey PaymentKey
  }
  deriving stock (Show)

genKeyPair :: IO KeyPair
genKeyPair = do
  sKey <- generateSigningKey AsPaymentKey
  return $ KeyPair sKey (getVerificationKey sKey)

-- | Make `AnyAddress` for mainnet
cardanoMainnetAddress :: KeyPair -> AddressAny
cardanoMainnetAddress KeyPair {vKey} =
  CAPI.toAddressAny $
    CAPI.makeShelleyAddress
      CAPI.Mainnet
      (CAPI.PaymentCredentialByKey (CAPI.verificationKeyHash vKey))
      CAPI.NoStakeAddress

-- | Get `String` representation of address on mainnet
mainnetAddress :: KeyPair -> String
mainnetAddress =
  showAddress . cardanoMainnetAddress

showAddress :: AddressAny -> String
showAddress = Text.unpack . CAPI.serialiseAddress
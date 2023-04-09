
module Test.Plutip.Internal.Keys (KeyPair, cardanoMainnetAddress, genKeyPair, mainnetAddress, saveKeyPair, showPkh) where

import Cardano.Api (AsType (AsPaymentKey), Key (VerificationKey, getVerificationKey, verificationKeyHash), PaymentKey, SigningKey, TextEnvelopeDescr, generateSigningKey, writeFileTextEnvelope, AddressAny)
import System.FilePath ((<.>), (</>))
import System.Directory
    ( createDirectoryIfMissing )
import qualified Cardano.Api as CAPI
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TE
import Control.Monad.Except (runExceptT, ExceptT (ExceptT))

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

showPkh :: KeyPair -> String 
showPkh = Text.unpack . TE.decodeUtf8 . CAPI.serialiseToRawBytesHex . verificationKeyHash . vKey

saveKeyPair :: FilePath -> KeyPair -> IO (Either (CAPI.FileError ()) ())
saveKeyPair dir kp@KeyPair {sKey, vKey} =
  let
    skeyDesc, vkeyDesc :: TextEnvelopeDescr
    skeyDesc = "Payment Signing Key"
    vkeyDesc = "Payment Verification Key"

    hash = showPkh kp

    skeyPath = dir </> "signing-key-" <> hash <.> "skey"
    vkeyPath = dir </> "verification-key-" <> hash <.> "vkey"

  in do
    createDirectoryIfMissing True dir
    runExceptT $ do
      ExceptT $ writeFileTextEnvelope skeyPath (Just skeyDesc) sKey
      ExceptT $ writeFileTextEnvelope vkeyPath (Just vkeyDesc) vKey


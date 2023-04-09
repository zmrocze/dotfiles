{-# LANGUAGE DerivingVia #-}

module Ledger.Certificate.Orphans where 

import Cardano.Api qualified as C
import Data.Aeson (ToJSON (toJSON), Value, decode', FromJSON)
import Codec.Serialise (deserialiseOrFail, Serialise (encode, decode), deserialise, serialise)
import GHC.Generics (Generic)
import Codec.CBOR.JSON (encodeValue, decodeValue)
import Data.Maybe (fromJust)
import Data.Aeson.Types (FromJSON(parseJSON))
import Cardano.Api (SerialiseAsCBOR, ToCBOR, StakeCredential, StakePoolParameters, EpochNo, GenesisKey, Hash, GenesisDelegateKey, MIRTarget, SerialiseAsRawBytes)
import Cardano.Api.Shelley (PoolId, VrfKey)
import Cardano.Ledger.Shelley.API (MIRPot)
import Data.Data (Typeable, Proxy)
import qualified Data.Aeson as Aeson
import qualified Data.Text.Encoding as Text


-- TODO:
-- przetestuj w replu 


-- Serialise JSON

-- instance Generic C.Certificate

deriving via (JSONByCBOR C.Certificate) instance (ToJSON C.Certificate)

newtype CBORForJSON = CBORForJSON { getJSON :: Value }

-- -- Defines CBOR Serialise class for Aeson's Value.
-- instance ToCBOR CBORForJSON where 
--     toCBOR = encodeValue . getJSON

-- instance FromCBOR CBORForJSON where
--     fromCBOR = CBORForJSON <$> decodeValue False

instance SerialiseAsCBOR CBORForJSON

-- Has Serialise => JSON instances.
newtype JSONByCBOR a = JSONByCBOR a

instance Serialise a => ToJSON (JSONByCBOR a) where
    toJSON (JSONByCBOR a) = getJSON $ deserialise $ serialise a

instance Serialise a => FromJSON (JSONByCBOR a) where
    parseJSON value = pure $ JSONByCBOR $ deserialise $ serialise $ CBORForJSON value

-- a -> Bytes
--      Bytes -> a

-- Value -> Bytes
--          Bytes -> Value 

-- instance ToJSON C.Certificate
-- instance ToJSON C.Certificate where
    -- toJSON a = deserialiseOrFail $ C.serialiseToCBOR a


data Certificate =

     -- Stake address certificates
     StakeAddressRegistrationCertificate   StakeCredential
   | StakeAddressDeregistrationCertificate StakeCredential
   | StakeAddressDelegationCertificate     StakeCredential PoolId

     -- Stake pool certificates
   | StakePoolRegistrationCertificate StakePoolParameters
   | StakePoolRetirementCertificate   PoolId EpochNo

     -- Special certificates
   | GenesisKeyDelegationCertificate (Hash GenesisKey)
                                     (Hash GenesisDelegateKey)
                                     (Hash VrfKey)
   | MIRCertificate MIRPot MIRTarget

  deriving stock (Eq, Show, Generic)
--   deriving anyclass SerialiseAsCBO

instance ToJSON Certificate

newtype UsingRawBytesHex a = UsingRawBytesHex a

instance (SerialiseAsRawBytes a, Typeable a) => FromJSON (UsingRawBytesHex a) where
  parseJSON =
    Aeson.withText tname $
      either fail pure . deserialiseFromRawBytesBase16 . Text.encodeUtf8
    where
      tname  = (tyConName . typeRepTyCon . typeRep) (Proxy :: Proxy a)

instance SerialiseAsRawBytes a => ToJSONKey (UsingRawBytesHex a) where
  toJSONKey =
    Aeson.toJSONKeyText $ \(UsingRawBytesHex x) -> serialiseToRawBytesHexText x

instance
  (SerialiseAsRawBytes a, Typeable a) => FromJSONKey (UsingRawBytesHex a) where

  fromJSONKey =
    Aeson.FromJSONKeyTextParser $
    either fail pure . deserialiseFromRawBytesBase16 . Text.encodeUtf8

instance SerialiseAsRawBytes a => ToJSON (UsingRawBytesHex a) where
    toJSON (UsingRawBytesHex x) = toJSON (serialiseToRawBytesHexText x)

deriving via C.UsingRawBytesHex instance (ToJSON, FromJSON) 

module Ledger.Certificate.Orphans where


import Cardano.Api qualified as C
import Data.Aeson (ToJSON (toJSON), Value, decode', FromJSON, ToJSONKey, FromJSONKey, defaultOptions, genericToEncoding)
import Codec.Serialise (deserialiseOrFail, Serialise (encode, decode), deserialise, serialise)
import GHC.Generics (Generic)
import Codec.CBOR.JSON (encodeValue, decodeValue)
import Data.Maybe (fromJust)
import Data.Aeson.Types (FromJSON(parseJSON))
import Cardano.Api (SerialiseAsCBOR, ToCBOR, StakeCredential, StakePoolParameters, EpochNo, GenesisKey, Hash, GenesisDelegateKey, MIRTarget, SerialiseAsRawBytes (deserialiseFromRawBytes), HasTypeProxy (proxyToAsType), serialiseToRawBytesHexText)
import Cardano.Api.Shelley (PoolId, VrfKey)
import Cardano.Ledger.Shelley.API (MIRPot)
import Data.Data (Typeable, Proxy (Proxy), typeRep, typeRepTyCon, tyConName)
import qualified Data.Aeson as Aeson
import qualified Data.Text.Encoding as Text
import qualified Data.ByteString.Base16 as Base16
import Data.ByteString (ByteString)
import PlutusPrelude (NFData)

instance Generic C.Certificate

instance NFData C.Certificate

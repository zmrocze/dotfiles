
module Ledger.Certificate.Orphans where


import Cardano.Api qualified as C
import Data.Aeson (ToJSON (toEncoding, toJSON), FromJSON (parseJSON))
import Codec.Serialise (Serialise (encode, decode))
import GHC.Generics (Generic)
import PlutusPrelude (NFData)
import Control.DeepSeq (NFData(rnf))
-- import Cardano.Api (ToCBOR, toCBOR)

instance Generic C.Certificate

instance NFData C.Certificate where 
    rnf = _

instance ToJSON C.Certificate where
    toJSON = _

instance FromJSON C.Certificate where 
    parseJSON = _

instance Serialise C.Certificate where 
    -- encode = toCBOR
    encode = _
    decode = _
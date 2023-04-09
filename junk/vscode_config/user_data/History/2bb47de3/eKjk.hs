
module Ledger.Certificate.Orphans where


import Cardano.Api qualified as C
import Data.Aeson (ToJSON (toEncoding), FromJSON (parseJSON))
import Codec.Serialise (Serialise)
import GHC.Generics (Generic)
import PlutusPrelude (NFData)

instance Generic C.Certificate

instance NFData C.Certificate

instance ToJSON C.Certificate where
    toEncoding = _

instance FromJSON C.Certificate where 
    parseJSON = _

instance Serialise C.Certificate
    -- serialise = serialiseToCBOR
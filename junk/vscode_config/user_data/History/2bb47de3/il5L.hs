
module Ledger.Certificate.Orphans where


import Cardano.Api qualified as C
import Data.Aeson (ToJSON, FromJSON)
import Codec.Serialise (Serialise)
import GHC.Generics (Generic)
import PlutusPrelude (NFData)

instance Generic C.Certificate

instance NFData C.Certificate

instance ToJSON C.Certificate where
    toEncoding = undefined

instance FromJSON C.Certificate

instance Serialise C.Certificate
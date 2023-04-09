{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Api.TxMetadata.Orphans where 
import Data.Aeson (ToJSON (toJSON), FromJSON, KeyValue ((.=)), object, pairs)
import Cardano.Api (TxMetadata(..), TxMetadataValue(..))
import Codec.Serialise (Serialise)
import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import Plutus.V1.Ledger.Api (LedgerBytes(LedgerBytes))
import Data.Aeson.Extras (encodeByteString)
import Data.Aeson.Types (ToJSON(toEncoding))

deriving stock instance Generic TxMetadata
deriving anyclass instance ToJSON TxMetadata
deriving anyclass instance FromJSON TxMetadata
deriving anyclass instance Serialise TxMetadata
deriving anyclass instance NFData TxMetadata

deriving stock instance Generic TxMetadataValue

-- Has to be defined by hand as there's no ToJSON ByteString.
instance ToJSON TxMetadataValue where 
    toEncoding (TxMetaMap ts) = pairs $ "map" .= toEncoding ts
    toEncoding (TxMetaList ls) = pairs $ "list" .= toEncoding ls
    toEncoding (TxMetaNumber i) = toEncoding i
    toEncoding (TxMetaBytes b) = pairs $ "bytes" .= toEncoding (encodeByteString b)
    toEncoding (TxMetaText t) = pairs $ "text" .= toJSON t
    
instance FromJSON TxMetadataValue where 
    -- parseJSON = withObject "TxMetadataValue" (o -> _) 

deriving anyclass instance Serialise TxMetadataValue
deriving anyclass instance NFData TxMetadataValue
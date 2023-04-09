{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Api.TxMetadata.Orphans where
import Data.Aeson (ToJSON (toJSON), FromJSON (parseJSON), KeyValue ((.=)), object, pairs, withObject, (.:), json', withScientific)
import Cardano.Api (TxMetadata(..), TxMetadataValue(..))
import Codec.Serialise (Serialise)
import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import Plutus.V1.Ledger.Api (LedgerBytes(LedgerBytes))
import Data.Aeson.Extras (encodeByteString, decodeByteString)
import Data.Aeson.Types (ToJSON(toEncoding))
import Control.Applicative ((<|>))

deriving stock instance Generic TxMetadata
deriving anyclass instance ToJSON TxMetadata
deriving anyclass instance FromJSON TxMetadata
deriving anyclass instance Serialise TxMetadata
deriving anyclass instance NFData TxMetadata

deriving stock instance Generic TxMetadataValue

-- Has to be defined by hand as there's no ToJSON ByteString.
instance ToJSON TxMetadataValue where
    toJSON (TxMetaMap ts) = object ["map" .= ts]
    toJSON (TxMetaList ls) = object ["list" .= ls]
    toJSON (TxMetaNumber i) = toJSON i
    toJSON (TxMetaBytes b) = object ["bytes" .= encodeByteString b]
    toJSON (TxMetaText t) = object ["text" .= toJSON t]

    toEncoding (TxMetaMap ts) = pairs $ "map" .= ts
    toEncoding (TxMetaList ls) = pairs $ "list" .= ls
    toEncoding (TxMetaNumber i) = toEncoding i
    toEncoding (TxMetaBytes b) = pairs $ "bytes" .= encodeByteString b
    toEncoding (TxMetaText t) = pairs $ "text" .= toJSON t

instance FromJSON TxMetadataValue where
    parseJSON =
        withObject "TxMetaMap" (\o -> TxMetaMap <$> o .: "map")
        <|> withObject "TxMetaMap" (\o -> TxMetaList <$> o .: "list")
        <|> withScientific "TxMetaNumber" (\o -> TxMetaNumber <$>  o)
        <|> withObject "TxMetaMap" (\o -> TxMetaBytes <$> (o .: "bytes" >>= decodeByteString))
        <|> withObject "TxMetaMap" (\o -> TxMetaText <$> o .: "text")

deriving anyclass instance Serialise TxMetadataValue
deriving anyclass instance NFData TxMetadataValue
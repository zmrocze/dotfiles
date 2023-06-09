-- File auto generated by purescript-bridge! --
module Ledger.Tx.Types.Tx where

import Prelude

import Control.Lazy (defer)
import Data.Argonaut (encodeJson, jsonNull)
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Decode.Aeson ((</$\>), (</*\>), (</\>))
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Encode.Aeson ((>$<), (>/\<))
import Data.Generic.Rep (class Generic)
import Data.Lens (Iso', Lens', Prism', iso, prism')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested ((/\))
import Ledger.Crypto (PubKey, Signature)
import Ledger.Slot (Slot)
import Ledger.Tx.Types.Certificate (Certificate)
import Ledger.Tx.Types.TxInput (TxInput)
import Ledger.Tx.Types.Withdrawal (Withdrawal)
import Plutus.V1.Ledger.Interval (Interval)
import Plutus.V1.Ledger.Scripts (DatumHash)
import Plutus.V1.Ledger.Tx (TxOut)
import Plutus.V1.Ledger.Value (Value)
import Type.Proxy (Proxy(Proxy))
import Data.Argonaut.Decode.Aeson as D
import Data.Argonaut.Encode.Aeson as E
import Data.Map as Map

newtype Tx = Tx
  { txInputs :: Array TxInput
  , txCollateral :: Array TxInput
  , txOutputs :: Array TxOut
  , txMint :: Value
  , txFee :: Value
  , txValidRange :: Interval Slot
  , txMintingScripts :: Map String String
  , txWithdrawals :: Array Withdrawal
  , txCertificates :: Array Certificate
  , txSignatures :: Map PubKey Signature
  , txScripts :: Map String String
  , txData :: Map DatumHash String
  , txMetadata :: Maybe String
  }

derive instance Eq Tx

instance Show Tx where
  show a = genericShow a

instance EncodeJson Tx where
  encodeJson = defer \_ -> E.encode $ unwrap >$<
    ( E.record
        { txInputs: E.value :: _ (Array TxInput)
        , txCollateral: E.value :: _ (Array TxInput)
        , txOutputs: E.value :: _ (Array TxOut)
        , txMint: E.value :: _ Value
        , txFee: E.value :: _ Value
        , txValidRange: E.value :: _ (Interval Slot)
        , txMintingScripts: (E.dictionary E.value E.value) :: _ (Map String String)
        , txWithdrawals: E.value :: _ (Array Withdrawal)
        , txCertificates: E.value :: _ (Array Certificate)
        , txSignatures: (E.dictionary E.value E.value) :: _ (Map PubKey Signature)
        , txScripts: (E.dictionary E.value E.value) :: _ (Map String String)
        , txData: (E.dictionary E.value E.value) :: _ (Map DatumHash String)
        , txMetadata: (E.maybe E.value) :: _ (Maybe String)
        }
    )

instance DecodeJson Tx where
  decodeJson = defer \_ -> D.decode $
    ( Tx <$> D.record "Tx"
        { txInputs: D.value :: _ (Array TxInput)
        , txCollateral: D.value :: _ (Array TxInput)
        , txOutputs: D.value :: _ (Array TxOut)
        , txMint: D.value :: _ Value
        , txFee: D.value :: _ Value
        , txValidRange: D.value :: _ (Interval Slot)
        , txMintingScripts: (D.dictionary D.value D.value) :: _ (Map String String)
        , txWithdrawals: D.value :: _ (Array Withdrawal)
        , txCertificates: D.value :: _ (Array Certificate)
        , txSignatures: (D.dictionary D.value D.value) :: _ (Map PubKey Signature)
        , txScripts: (D.dictionary D.value D.value) :: _ (Map String String)
        , txData: (D.dictionary D.value D.value) :: _ (Map DatumHash String)
        , txMetadata: (D.maybe D.value) :: _ (Maybe String)
        }
    )

derive instance Generic Tx _

derive instance Newtype Tx _

--------------------------------------------------------------------------------

_Tx :: Iso' Tx { txInputs :: Array TxInput, txCollateral :: Array TxInput, txOutputs :: Array TxOut, txMint :: Value, txFee :: Value, txValidRange :: Interval Slot, txMintingScripts :: Map String String, txWithdrawals :: Array Withdrawal, txCertificates :: Array Certificate, txSignatures :: Map PubKey Signature, txScripts :: Map String String, txData :: Map DatumHash String, txMetadata :: Maybe String }
_Tx = _Newtype

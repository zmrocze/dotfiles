{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia        #-}

module Ledger.Tx.Types.Certificate where

import Codec.Serialise (Serialise)
import Control.DeepSeq (NFData)
import Control.Lens
import Data.Aeson (FromJSON, ToJSON)
import Data.ByteArray qualified as BA
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set qualified as Set
import GHC.Generics (Generic)
import Ledger.Crypto
import Ledger.DCert.Orphans ()
import Ledger.Scripts (datumHash, mintingPolicyHash, validatorHash)
import Ledger.Slot
import Ledger.Tx.Orphans ()
import Plutus.V1.Ledger.Api (BuiltinByteString, Credential, DCert)
import Plutus.V1.Ledger.Scripts
import Plutus.V1.Ledger.Tx (TxIn (..), TxInType (..), TxOut (txOutValue), TxOutRef, txOutDatum)
import Plutus.V1.Ledger.Value as V
import PlutusTx.Lattice
import Prettyprinter (Pretty (pretty), hang, viaShow, vsep, (<+>))

data Certificate = Certificate
  { certificateDcert    :: DCert
  , certificateRedeemer :: Maybe Redeemer           -- ^ redeemer for script credential
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Serialise, NFData)

instance Pretty Certificate where
    pretty = viaShow

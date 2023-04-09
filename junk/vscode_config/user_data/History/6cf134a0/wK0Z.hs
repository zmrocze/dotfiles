{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeApplications   #-}

-- {-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE RecordWildCards    #-}
-- {-# OPTIONS_GHC -Wno-unused-imports #-}

module Ledger.CardanoTx
    ( module Export
    -- * ChainIndexTxOut
    , toTxOut
    , fromTxOut
    -- ** Lenses and Prisms
    , ciTxOutAddress
    , ciTxOutValue
    , ciTxOutDatum
    , ciTxOutValidator
    , _PublicKeyChainIndexTxOut
    , _ScriptChainIndexTxOut
    , CardanoTx(..)
    , onCardanoTx
    , mergeCardanoTxWith
    , cardanoTxMap
    , getCardanoTxId
    , getCardanoTxInputs
    , getCardanoTxOutRefs
    , getCardanoTxUnspentOutputsTx
    , getCardanoTxFee
    , SomeCardanoApiTx(..)
    , ToCardanoError(..)
    ) where

import Cardano.Api qualified as C
import Control.Lens (makeLenses, makePrisms)
import Data.Aeson (FromJSON, ToJSON)
import Data.Map (Map)
import Data.OpenApi qualified as OpenApi
import GHC.Generics (Generic)
import Ledger.Address (Address)
import Ledger.Tx.CardanoAPI (SomeCardanoApiTx (SomeTx), ToCardanoError (..))
import Ledger.Tx.CardanoAPI qualified as CardanoAPI
import Ledger.Tx.Types.Tx as Export
import Plutus.Script.Utils.V1.Scripts (datumHash)
import Plutus.V1.Ledger.Api (Credential (PubKeyCredential, ScriptCredential), Datum, DatumHash, Validator,
                             ValidatorHash, Value, addressCredential)
import Plutus.V1.Ledger.Tx as Export
import Prettyprinter (Pretty (pretty), hang, vsep, (<+>))
import Ledger.Tx (txId, fillTxInputWitnesses, txOutRefs, unspentOutputsTx)
import Codec.Serialise (Serialise)

-- | Transaction output that comes from a chain index query.
--
-- It is defined here instead of the plutus-chain-index because plutus-ledger
-- uses that datatype, and plutus-ledger can't depend on plutus-chain-index
-- because of a cyclic dependency.
--
-- This datatype was created in order to be used in
-- 'Ledger.Constraints.processConstraint', specifically with the constraints
-- 'MustSpendPubKeyOutput' and 'MustSpendScriptOutput'.
--
-- TODO Add 'Either DatumHash Datum' field for 'PublicKeyChainIndexTxOut'.
data ChainIndexTxOut =
    PublicKeyChainIndexTxOut { _ciTxOutAddress :: Address
                             , _ciTxOutValue   :: Value
                             }
  | ScriptChainIndexTxOut { _ciTxOutAddress   :: Address
                          , _ciTxOutValidator :: Either ValidatorHash Validator
                          , _ciTxOutDatum     :: Either DatumHash Datum
                          , _ciTxOutValue     :: Value
                          }
  deriving (Show, Eq, Serialise, Generic, ToJSON, FromJSON, OpenApi.ToSchema)

makeLenses ''ChainIndexTxOut
makePrisms ''ChainIndexTxOut

-- | Converts a transaction output from the chain index to the plutus-ledger-api
-- transaction output.
--
-- Note that converting from 'ChainIndexTxOut' to 'TxOut' and back to
-- 'ChainIndexTxOut' loses precision ('Datum' and 'Validator' are changed to 'DatumHash' and 'ValidatorHash' respectively)
toTxOut :: ChainIndexTxOut -> TxOut
toTxOut (PublicKeyChainIndexTxOut addr v)          = TxOut addr v Nothing
toTxOut (ScriptChainIndexTxOut addr _ (Left dh) v) = TxOut addr v (Just dh)
toTxOut (ScriptChainIndexTxOut addr _ (Right d) v) = TxOut addr v (Just $ datumHash d)

-- | Converts a plutus-ledger-api transaction output to the chain index
-- transaction output.
fromTxOut :: TxOut -> Maybe ChainIndexTxOut
fromTxOut TxOut { txOutAddress, txOutValue, txOutDatumHash } =
  case addressCredential txOutAddress of
    PubKeyCredential _ -> pure $ PublicKeyChainIndexTxOut txOutAddress txOutValue
    ScriptCredential vh ->
      txOutDatumHash >>= \dh ->
        pure $ ScriptChainIndexTxOut txOutAddress (Left vh) (Left dh) txOutValue

instance Pretty ChainIndexTxOut where
    pretty PublicKeyChainIndexTxOut {_ciTxOutAddress, _ciTxOutValue} =
                hang 2 $ vsep ["-" <+> pretty _ciTxOutValue <+> "addressed to", pretty _ciTxOutAddress]
    pretty ScriptChainIndexTxOut {_ciTxOutAddress, _ciTxOutValue} =
                hang 2 $ vsep ["-" <+> pretty _ciTxOutValue <+> "addressed to", pretty _ciTxOutAddress]


{- Note [Why we have the Both constructor in CardanoTx]

We want to do validation with both the emulator and with the cardano-ledger library, at least as long
as we don't have Phase2 validation errors via the cardano-ledger library.

To do that we need the required signers which are only available in UnbalancedTx during balancing.
So during balancing we can create the SomeCardanoApiTx, while proper validation can only happen in
Wallet.Emulator.Chain.validateBlock, since that's when we know the right Slot number. This means that
we need both transaction types in the path from balancing to validateBlock. -}

data CardanoTx
    = EmulatorTx Tx
    | CardanoApiTx SomeCardanoApiTx
    | Both Tx SomeCardanoApiTx
    deriving (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON, OpenApi.ToSchema)

instance Pretty CardanoTx where
    pretty = onCardanoTx pretty (pretty . getCardanoApiTxId)

onCardanoTx :: (Tx -> r) -> (SomeCardanoApiTx -> r) -> CardanoTx -> r
onCardanoTx l r = mergeCardanoTxWith l r const

mergeCardanoTxWith :: (Tx -> a) -> (SomeCardanoApiTx -> a) -> (a -> a -> a) -> CardanoTx -> a
mergeCardanoTxWith l _ _ (EmulatorTx tx)    = l tx
mergeCardanoTxWith l r m (Both tx ctx)      = m (l tx) (r ctx)
mergeCardanoTxWith _ r _ (CardanoApiTx ctx) = r ctx

cardanoTxMap :: (Tx -> Tx) -> (SomeCardanoApiTx -> SomeCardanoApiTx) -> CardanoTx -> CardanoTx
cardanoTxMap l _ (EmulatorTx tx)    = EmulatorTx (l tx)
cardanoTxMap l r (Both tx ctx)      = Both (l tx) (r ctx)
cardanoTxMap _ r (CardanoApiTx ctx) = CardanoApiTx (r ctx)

getCardanoTxId :: CardanoTx -> TxId
getCardanoTxId = onCardanoTx txId getCardanoApiTxId

getCardanoApiTxId :: SomeCardanoApiTx -> TxId
getCardanoApiTxId (SomeTx (C.Tx body _) _) = CardanoAPI.fromCardanoTxId $ C.getTxId body

getCardanoTxInputs :: CardanoTx -> [TxIn]
getCardanoTxInputs = onCardanoTx (\tx -> map (fillTxInputWitnesses tx) $ txInputs tx)
    (\(SomeTx (C.Tx (C.TxBody C.TxBodyContent {..}) _) _) ->
        fmap ((`TxIn` Nothing) . CardanoAPI.fromCardanoTxIn . fst) txIns)

getCardanoTxOutRefs :: CardanoTx -> [(TxOut, TxOutRef)]
getCardanoTxOutRefs = onCardanoTx txOutRefs CardanoAPI.txOutRefs

getCardanoTxUnspentOutputsTx :: CardanoTx -> Map TxOutRef TxOut
getCardanoTxUnspentOutputsTx = onCardanoTx unspentOutputsTx CardanoAPI.unspentOutputsTx

getCardanoTxFee :: CardanoTx -> Value
getCardanoTxFee = onCardanoTx txFee (\_ -> error "Ledger.Tx.getCardanoTxFee: Expecting a mock tx, not an Alonzo tx")

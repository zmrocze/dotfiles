{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeApplications   #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE RecordWildCards    #-}

module Ledger.Tx
    ( module Export
    -- * ChainIndexTxOut
    , ChainIndexTxOut(..)
    , TxOutTx(..)
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
    -- * Transactions
    , addSignature
    , addSignature'
    , pubKeyTxOut
    , updateUtxo
    , txOutRefs
    , unspentOutputsTx
    -- * Hashing transactions
    , txId
    -- * Helper functions
    , lookupSignature
    , lookupMintingScripts
    , lookupScript
    , lookupValidator
    , lookupMintingPolicy
    , lookupStakeValidator
    , fillTxInputWitnesses
    , pubKeyTxInput
    , addMintingPolicy
    , addScriptTxInput
    , validValuesTx
    , strip
    , txOutTxDatum
    , spentOutputs
    , updateUtxoCollateral
    ) where

import Cardano.Api qualified as C
import Cardano.Crypto.Hash (SHA256, digest)
import Cardano.Crypto.Wallet qualified as Crypto
import Codec.CBOR.Write qualified as Write
import Codec.Serialise (Serialise (encode))
import Control.Lens (At (at), makeLenses, makePrisms, (&), (?~))
import Data.Aeson (FromJSON, ToJSON)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (isJust)
import Data.OpenApi qualified as OpenApi
import Data.Proxy (Proxy (Proxy))
import Data.Set qualified as Set
import GHC.Generics (Generic)
import Ledger.Address (Address, PaymentPubKey, StakePubKey, pubKeyAddress)
import Ledger.Crypto (Passphrase, signTx, signTx', toPublicKey)
import Ledger.Orphans ()
import Ledger.Tx.CardanoAPI (SomeCardanoApiTx (SomeTx), ToCardanoError (..))
import Ledger.Tx.CardanoAPI qualified as CardanoAPI
import Ledger.Tx.Types.Tx as Export
import Plutus.Script.Utils.V1.Scripts (datumHash)
import Plutus.V1.Ledger.Api (Credential (PubKeyCredential, ScriptCredential), Datum, DatumHash, Validator,
                             ValidatorHash, Value, addressCredential, toBuiltin)
import Plutus.V1.Ledger.Tx as Export
import Prettyprinter (Pretty (pretty), braces, colon, hang, nest, viaShow, vsep, (<+>))

type PrivateKey = Crypto.XPrv

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

-- | Compute the id of a transaction.
txId :: Tx -> TxId
-- Double hash of a transaction, excluding its witnesses.
txId tx = TxId $ toBuiltin
               $ digest (Proxy @SHA256)
               $ digest (Proxy @SHA256)
               (Write.toStrictByteString $ encode $ strip tx)

-- | Update a map of unspent transaction outputs and signatures based on the inputs
--   and outputs of a transaction.
updateUtxo :: Tx -> Map TxOutRef TxOut -> Map TxOutRef TxOut
updateUtxo tx unspent = (unspent `Map.withoutKeys` Set.fromList (spentOutputs tx)) `Map.union` unspentOutputsTx tx

-- | A list of a transaction's outputs paired with a 'TxOutRef's referring to them.
txOutRefs :: Tx -> [(TxOut, TxOutRef)]
txOutRefs t = mkOut <$> zip [0..] (txOutputs t) where
    mkOut (i, o) = (o, TxOutRef (txId t) i)

-- | The unspent outputs of a transaction.
unspentOutputsTx :: Tx -> Map TxOutRef TxOut
unspentOutputsTx t = Map.fromList $ fmap f $ zip [0..] $ txOutputs t where
    f (idx, o) = (TxOutRef (txId t) idx, o)

-- | Create a transaction output locked by a public payment key and optionnaly a public stake key.
pubKeyTxOut :: Value -> PaymentPubKey -> Maybe StakePubKey -> TxOut
pubKeyTxOut v pk sk = TxOut (pubKeyAddress pk sk) v Nothing

-- | Sign the transaction with a 'PrivateKey' and passphrase (ByteString) and add the signature to the
--   transaction's list of signatures.
addSignature :: PrivateKey -> Passphrase -> Tx -> Tx
addSignature privK passPhrase tx = tx & signatures . at pubK ?~ sig where
    sig = signTx (txId tx) privK passPhrase
    pubK = toPublicKey privK

-- | Sign the transaction with a 'PrivateKey' that has no passphrase and add the signature to the
--   transaction's list of signatures
addSignature' :: PrivateKey -> Tx -> Tx
addSignature' privK tx = tx & signatures . at pubK ?~ sig where
    sig = signTx' (txId tx) privK
    pubK = toPublicKey privK

lookupSignature :: PubKey -> Tx -> Maybe Signature
lookupSignature s Tx{txSignatures} = Map.lookup s txSignatures

-- | Get MintingPolicy scripts for MintingPolicyHash'es included in the transaction,
-- Nothing means the transaction misses given script witness.
lookupMintingScripts :: Tx -> [Maybe MintingPolicy]
lookupMintingScripts Tx{txMintingScripts, txScripts} =
    map (\mph -> MintingPolicy <$> Map.lookup (toScriptHash mph) txScripts ) (Map.keys txMintingScripts)
    where
        toScriptHash (MintingPolicyHash b) = ScriptHash b

lookupScript :: Map ScriptHash Script -> ScriptHash -> Maybe Script
lookupScript txScripts hash  = Map.lookup hash txScripts

lookupValidator :: Map ScriptHash Script -> ValidatorHash -> Maybe Validator
lookupValidator txScripts = fmap Validator . lookupScript txScripts . toScriptHash
    where
        toScriptHash (ValidatorHash b) = ScriptHash b

lookupMintingPolicy :: Map ScriptHash Script -> MintingPolicyHash -> Maybe MintingPolicy
lookupMintingPolicy txScripts = fmap MintingPolicy . lookupScript txScripts . toScriptHash
    where
        toScriptHash (MintingPolicyHash b) = ScriptHash b

lookupStakeValidator :: Map ScriptHash Script -> StakeValidatorHash -> Maybe StakeValidator
lookupStakeValidator txScripts = fmap StakeValidator . lookupScript txScripts . toScriptHash
    where
        toScriptHash (StakeValidatorHash b) = ScriptHash b

-- | Translate TxInput to TxIn taking script and datum witnesses from Tx.
fillTxInputWitnesses :: Tx -> TxInput -> TxIn
fillTxInputWitnesses tx (TxInput outRef inType) = case inType of
    TxConsumePublicKeyAddress -> TxIn outRef (Just ConsumePublicKeyAddress)
    TxConsumeSimpleScriptAddress -> TxIn outRef (Just ConsumeSimpleScriptAddress)
    TxConsumeScriptAddress redeemer vlh dh -> TxIn outRef $ do
        datum <- Map.lookup dh (txData tx)
        validator <- lookupValidator (txScripts tx) vlh
        Just $ ConsumeScriptAddress validator redeemer datum

pubKeyTxInput :: TxOutRef -> TxInput
pubKeyTxInput outRef = TxInput outRef TxConsumePublicKeyAddress

-- | Add minting policy together with the redeemer into txMintingScripts and txScripts accordingly.
addMintingPolicy :: MintingPolicy -> Redeemer -> Tx -> Tx
addMintingPolicy vl@(MintingPolicy script) rd tx@Tx{txMintingScripts, txScripts} = tx
    {txMintingScripts = Map.insert mph rd txMintingScripts,
     txScripts = Map.insert (ScriptHash b) script txScripts}
    where
        mph@(MintingPolicyHash b) = mintingPolicyHash vl

-- | Add minting policy together with the redeemer into txMintingScripts and txScripts accordingly.
addScriptTxInput :: TxOutRef -> Validator -> Redeemer -> Datum -> Tx -> Tx
addScriptTxInput outRef vl@(Validator script) rd dt tx@Tx{txInputs, txScripts, txData} = tx
    {txInputs = TxInput outRef (TxConsumeScriptAddress rd vlHash dtHash) : txInputs,
     txScripts = Map.insert (ScriptHash b) script txScripts,
     txData = Map.insert dtHash dt txData}
    where
        dtHash = datumHash dt
        vlHash@(ValidatorHash b) = validatorHash vl

-- | Check that all values in a transaction are non-negative.
validValuesTx :: Tx -> Bool
validValuesTx Tx{..}
  = all (nonNegative . txOutValue) txOutputs  && nonNegative txFee
    where
      nonNegative i = V.geq i mempty

-- THIS IS PROBABLY DEPRECATED
-- | A transaction without witnesses for its inputs.
data TxStripped = TxStripped {
    txStrippedInputs  :: Set.Set TxOutRef,
    -- ^ The inputs to this transaction, as transaction output references only.
    txStrippedOutputs :: [TxOut],
    -- ^ The outputs of this transation.
    txStrippedMint    :: !Value,
    -- ^ The 'Value' minted by this transaction.
    txStrippedFee     :: !Value
    -- ^ The fee for this transaction.
    } deriving (Show, Eq, Generic, Serialise)

-- THIS IS PROBABLY DEPRECATED
strip :: Tx -> TxStripped
strip Tx{..} = TxStripped (Set.fromList i) txOutputs txMint txFee where
    i = map txInputRef txInputs

-- | A 'TxOut' along with the 'Tx' it comes from, which may have additional information e.g.
-- the full data script that goes with the 'TxOut'.
data TxOutTx = TxOutTx { txOutTxTx :: Tx, txOutTxOut :: TxOut }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (Serialise, ToJSON, FromJSON)

txOutTxDatum :: TxOutTx -> Maybe Datum
txOutTxDatum (TxOutTx tx out) = txOutDatum out >>= (`Map.lookup` txData tx)

-- | The transaction output references consumed by a transaction.
spentOutputs :: Tx -> [TxOutRef]
spentOutputs = map txInputRef . txInputs

-- | Update a map of unspent transaction outputs and signatures
--   for a failed transaction using its collateral inputs.
updateUtxoCollateral :: Tx -> Map TxOutRef TxOut -> Map TxOutRef TxOut
updateUtxoCollateral tx unspent = unspent `Map.withoutKeys` (Set.fromList . map txInputRef . txCollateral $ tx)

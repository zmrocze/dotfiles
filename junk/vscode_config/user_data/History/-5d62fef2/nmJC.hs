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
{-| 
    Defines helper functions acting on `Tx`. Reexported from Ledger.Tx.
-}
module Ledger.Tx.Internal
    ( module Export
    -- * Transactions
    , addSignature
    , addSignature'
    , pubKeyTxOut
    , updateUtxo
    , txOutRefs
    , unspentOutputsTx
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
    , spentOutputs
    , updateUtxoCollateral
    ) where

import Cardano.Crypto.Wallet qualified as Crypto
import Control.Lens (at, makeLenses, makePrisms, (&), (?~))
import Data.Aeson (FromJSON, ToJSON)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (isJust)
import Data.OpenApi qualified as OpenApi
import Data.Proxy (Proxy (Proxy))
import Data.Set qualified as Set
import GHC.Generics (Generic)
import Ledger.Address (Address, PaymentPubKey, StakePubKey, pubKeyAddress)
import Ledger.Crypto (Passphrase, PubKey, Signature, signTx, signTx', toPublicKey)
import Ledger.Orphans ()
import Ledger.Tx.Types as Export
import Ledger.Value qualified as V
import Plutus.Script.Utils.V1.Scripts (StakeValidator, StakeValidatorHash, datumHash, mintingPolicyHash, validatorHash)
import Plutus.V1.Ledger.Api (Credential (PubKeyCredential, ScriptCredential), Datum, DatumHash, MintingPolicy,
                             MintingPolicyHash (MintingPolicyHash), Redeemer, Validator, ValidatorHash, Value,
                             addressCredential, toBuiltin)
import Plutus.V1.Ledger.Scripts (MintingPolicy (MintingPolicy), Script, ScriptHash (ScriptHash),
                                 StakeValidator (StakeValidator), StakeValidatorHash (StakeValidatorHash),
                                 Validator (Validator), ValidatorHash (ValidatorHash))
import Plutus.V1.Ledger.Tx as Export
import Prettyprinter (Pretty (pretty), braces, colon, hang, nest, viaShow, vsep, (<+>))

-- | The transaction output references consumed by a transaction.
spentOutputs :: Tx -> [TxOutRef]
spentOutputs = map txInputRef . txInputs

-- | Update a map of unspent transaction outputs and signatures
--   for a failed transaction using its collateral inputs.
updateUtxoCollateral :: Tx -> Map TxOutRef TxOut -> Map TxOutRef TxOut
updateUtxoCollateral tx unspent = unspent `Map.withoutKeys` (Set.fromList . map txInputRef . txCollateral $ tx)

-- | Check that all values in a transaction are non-negative.
validValuesTx :: Tx -> Bool
validValuesTx Tx{..}
  = all (nonNegative . txOutValue) txOutputs  && nonNegative txFee
    where
      nonNegative i = V.geq i mempty

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

type PrivateKey = Crypto.XPrv

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
fillTxInputWitnesses tx (TxInput outRef _inType) = case _inType of
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

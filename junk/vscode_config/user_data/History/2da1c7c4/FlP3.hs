{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | An index of unspent transaction outputs, and some functions for validating
--   transactions using the index.
module Ledger.Index(
    -- * Types for transaction validation based on UTXO index
    ValidationMonad,
    ValidationCtx(..),
    UtxoIndex(..),
    insert,
    insertCollateral,
    insertBlock,
    initialise,
    Validation(..),
    runValidation,
    lookup,
    lkpValue,
    lkpTxOut,
    lkpOutputs,
    ValidationError(..),
    ValidationErrorInPhase,
    ValidationPhase(..),
    EmulatorEra,
    InOutMatch(..),
    minFee,
    maxFee,
    minAdaTxOut,
    minLovelaceTxOut,
    mkTxInfo,
    -- * Actual validation
    validateTransaction,
    validateTransactionOffChain,
    checkValidInputs,
    -- * Script validation events
    ScriptType(..),
    ScriptValidationEvent(..),
    Api.ExBudget(..),
    Api.ExCPU(..),
    Api.ExMemory(..),
    Api.SatInt,
    ValidatorMode(..),
    getScript
    ) where

import Cardano.Api (Lovelace (..))
import Prelude hiding (lookup)

import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Crypto (StandardCrypto)
import Control.Lens (toListOf, view, (^.))
import Control.Lens.Indexed (iforM_)

import Codec.Serialise (Serialise)
import Control.DeepSeq (NFData)
import Control.Lens (toListOf, view, (^.))
import Control.Monad
import Control.Monad.Except (ExceptT, MonadError (..), runExcept, runExceptT)
import Control.Monad.Reader (MonadReader (..), ReaderT (..), ask)
import Control.Monad.Writer (MonadWriter, Writer, runWriter, tell)
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Either (fromRight)
import Data.Foldable (asum, fold, foldl', for_, traverse_)
import Data.Functor ((<&>))
import Data.Map qualified as Map
import Data.OpenApi.Schema qualified as OpenApi
import Data.Set qualified as Set
import Data.Text (Text)
import GHC.Generics (Generic)
import Ledger.Blockchain
import Ledger.Crypto
import Ledger.Index.Internal
import Ledger.Orphans ()
import Ledger.Params (Params (pSlotConfig))
import Ledger.TimeSlot qualified as TimeSlot
import Ledger.Tx (CardanoTx (..), txId, updateUtxoCollateral, TxInput (TxInput, txInputRef), TxOut, Tx(..), inputs, signatures, datumWitnesses, lookupMintingPolicy, fillTxInputWitnesses)
import Ledger.Validation (evaluateMinLovelaceOutput, fromPlutusTxOutUnsafe)
import Plutus.Script.Utils.V1.Scripts
import Plutus.V1.Ledger.Ada (Ada)
import Plutus.V1.Ledger.Ada qualified as Ada
import Plutus.V1.Ledger.Address (Address (Address, addressCredential))
import Plutus.V1.Ledger.Api qualified as Api
import Plutus.V1.Ledger.Contexts (ScriptContext (..), ScriptPurpose (..), TxInfo (..), TxOut (txOutValue))
import Plutus.V1.Ledger.Contexts qualified as Validation
import Plutus.V1.Ledger.Credential (Credential (..))
import Plutus.V1.Ledger.Interval qualified as Interval
import Plutus.V1.Ledger.Scripts
import Plutus.V1.Ledger.Scripts qualified as Scripts
import Plutus.V1.Ledger.Slot qualified as Slot
import Plutus.V1.Ledger.Tx (TxOutRef, TxIn (txInType), TxInType (ConsumeScriptAddress, ConsumePublicKeyAddress), txOutDatumHash, txOutAddress, txInRef)
import Plutus.V1.Ledger.TxId
import Plutus.V1.Ledger.Value qualified as V
import PlutusPrelude (first)
import PlutusTx (toBuiltinData)
import PlutusTx.Numeric qualified as P

-- | Context for validating transactions. We need access to the unspent
--   transaction outputs of the blockchain, and we can throw 'ValidationError's.
type ValidationMonad m = (MonadReader ValidationCtx m, MonadError ValidationError m, MonadWriter [ScriptValidationEvent] m)

data ValidationCtx = ValidationCtx { vctxIndex :: UtxoIndex, vctxParams :: Params }

-- | Create an index of all UTxOs on the chain.
initialise :: Blockchain -> UtxoIndex
initialise = UtxoIndex . unspentOutputs

-- | Update the index for the addition of a transaction.
insert :: CardanoTx -> UtxoIndex -> UtxoIndex
insert tx = UtxoIndex . updateUtxo tx . getIndex

-- | Update the index for the addition of only the collateral inputs of a failed transaction.
insertCollateral :: CardanoTx -> UtxoIndex -> UtxoIndex
insertCollateral tx = UtxoIndex . updateUtxoCollateral tx . getIndex

-- | Update the index for the addition of a block.
insertBlock :: Block -> UtxoIndex -> UtxoIndex
insertBlock blck i = foldl' (flip (eitherTx insertCollateral insert)) i blck

-- | Find an unspent transaction output by the 'TxOutRef' that spends it.
lookup :: MonadError ValidationError m => TxOutRef -> UtxoIndex -> m TxOut
lookup i index = case Map.lookup i $ getIndex index of
    Just t  -> pure t
    Nothing -> throwError $ TxOutRefNotFound i

-- | A monad for running transaction validation inside, which is an instance of 'ValidationMonad'.
newtype Validation a = Validation { _runValidation :: (ReaderT ValidationCtx (ExceptT ValidationError (Writer [ScriptValidationEvent]))) a }
    deriving newtype (Functor, Applicative, Monad, MonadReader ValidationCtx, MonadError ValidationError, MonadWriter [ScriptValidationEvent])

-- | Run a 'Validation' on a 'UtxoIndex'.
runValidation :: Validation (Maybe ValidationErrorInPhase) -> ValidationCtx -> (Maybe ValidationErrorInPhase, [ScriptValidationEvent])
runValidation l ctx = runWriter $ fmap (either (\e -> Just (Phase1, e)) id) $ runExceptT $ runReaderT (_runValidation l) ctx

-- | Determine the unspent value that a ''TxOutRef' refers to.
lkpValue :: ValidationMonad m => TxOutRef -> m V.Value
lkpValue = fmap txOutValue . lkpTxOut

-- | Find an unspent transaction output by its reference. Assumes that the
--   output for this reference exists. If you want to handle the lookup error
--   you can use 'runLookup'.
lkpTxOut :: ValidationMonad m => TxOutRef -> m TxOut
lkpTxOut t = lookup t . vctxIndex =<< ask

-- | Validate a transaction in a 'ValidationMonad' context.
validateTransaction :: ValidationMonad m
    => Slot.Slot
    -> Tx
    -> m (Maybe ValidationErrorInPhase)
validateTransaction h t = do
    -- Phase 1 validation
    checkSlotRange h t
    _ <- lkpOutputs $ toListOf (inputs . scriptTxInputs) t

    -- see note [Minting of Ada]
    emptyUtxoSet <- reader (Map.null . getIndex . vctxIndex)
    unless emptyUtxoSet (checkTransactionFee t)

    validateTransactionOffChain t

validateTransactionOffChain :: ValidationMonad m
    => Tx
    -> m (Maybe ValidationErrorInPhase)
validateTransactionOffChain t = do
    checkValuePreserved t
    checkPositiveValues t
    checkMinAdaInTxOutputs t
    checkFeeIsAda t

    -- see note [Minting of Ada]
    emptyUtxoSet <- reader (Map.null . getIndex . vctxIndex)
    unless emptyUtxoSet (checkMintingAuthorised t)

    checkValidInputs (toListOf (inputs . pubKeyTxInputs)) t
    checkValidInputs (view collateralInputs) t

    (do
        -- Phase 2 validation
        checkValidInputs (toListOf (inputs . scriptTxInputs)) t
        unless emptyUtxoSet (checkMintingScripts t)

        pure Nothing
        ) `catchError` (\e -> pure (Just (Phase2, e)))

-- | Check that a transaction can be validated in the given slot.
checkSlotRange :: ValidationMonad m => Slot.Slot -> Tx -> m ()
checkSlotRange sl tx =
    if Interval.member sl (txValidRange tx)
    then pure ()
    else throwError $ CurrentSlotOutOfRange sl

-- | Check if the inputs of the transaction consume outputs that exist, and
--   can be unlocked by the signatures or validator scripts of the inputs.
checkValidInputs :: ValidationMonad m => (Tx -> [TxInput]) -> Tx -> m ()
checkValidInputs getInputs tx = do
    let tid = txId tx
        sigs = tx ^. signatures
    outs <- map (first $ fillTxInputWitnesses tx) <$> lkpOutputs (getInputs tx)
    matches <- traverse (uncurry (matchInputOutput tid sigs)) outs
    vld     <- mkTxInfo tx
    traverse_ (checkMatch vld) matches

-- | Match each input of the transaction with the output that it spends.
lkpOutputs :: ValidationMonad m => [TxInput] -> m [(TxInput, TxOut)]
lkpOutputs = traverse (\t -> traverse (lkpTxOut . txInputRef) (t, t))

{- note [Minting of Ada]

'checkMintingAuthorised' will never allow a transaction that mints Ada.
Ada's currency symbol is the empty bytestring, and it can never be matched by a
validator script whose hash is its symbol.

Therefore 'checkMintingAuthorised' should not be applied to the first transaction in
the blockchain.

-}

-- | Check whether each currency minted by the transaction is matched by
--   a corresponding minting policy script (in the form of a pay-to-script
--   output of the currency's address).
--
checkMintingAuthorised :: ValidationMonad m => Tx -> m ()
checkMintingAuthorised tx =
    let
        mintedCurrencies = V.symbols (txMint tx)

        mpsScriptHashes = Scripts.MintingPolicyHash . V.unCurrencySymbol <$> mintedCurrencies

        lockingScripts = Map.keys $ txMintingScripts tx

        mintedWithoutScript = filter (\c -> c `notElem` lockingScripts) mpsScriptHashes
    in
        traverse_ (throwError . MintWithoutScript) mintedWithoutScript

checkMintingScripts :: forall m . ValidationMonad m => Tx -> m ()
checkMintingScripts tx = do
    txinfo <- mkTxInfo tx
    forM_ (Map.assocs $ txMintingScripts tx) $ \(mph, red) -> do
        let cs :: V.CurrencySymbol
            cs = V.mpsSymbol mph
            ctx :: Context
            ctx = Context $ toBuiltinData $ ScriptContext { scriptContextPurpose = Minting cs, scriptContextTxInfo = txinfo }

        vl <- case lookupMintingPolicy (txScripts tx) mph of
            Just vl | mintingPolicyHash vl == mph -> pure vl
            _                                             -> throwError $ MintWithoutScript mph

        case runExcept $ runMintingPolicyScript ctx vl red of
            Left e  -> do
                tell [mpsValidationEvent ctx vl red (Left e)]
                throwError $ ScriptFailure e
            res -> tell [mpsValidationEvent ctx vl red res]

-- | A matching pair of transaction input and transaction output, ensuring that they are of matching types also.
data InOutMatch =
    ScriptMatch
        TxOutRef
        Validator
        Redeemer
        Datum
    | PubKeyMatch TxId PubKey Signature
    deriving (Eq, Ord, Show)

-- | Match a transaction input with the output that it consumes, ensuring that
--   both are of the same type (pubkey or pay-to-script).
matchInputOutput :: ValidationMonad m
    => TxId
    -- ^ Hash of the transaction that is being verified
    -> Map.Map PubKey Signature
    -- ^ Signatures provided with the transaction
    -> TxIn
    -- ^ Input that allegedly spends the output
    -> TxOut
    -- ^ The unspent transaction output we are trying to unlock
    -> m InOutMatch
matchInputOutput txid mp txin txo = case (txInType txin, txOutDatumHash txo, txOutAddress txo) of
    (Just (ConsumeScriptAddress v r d), Just dh, Address{addressCredential=ScriptCredential vh}) -> do
        unless (datumHash d == dh) $ throwError $ InvalidDatumHash d dh
        unless (validatorHash v == vh) $ throwError $ InvalidScriptHash v vh

        pure $ ScriptMatch (txInRef txin) v r d
    (Just ConsumePublicKeyAddress, _, Address{addressCredential=PubKeyCredential pkh}) ->
        let sigMatches = flip fmap (Map.toList mp) $ \(pk,sig) ->
                if pubKeyHash pk == pkh
                then Just (PubKeyMatch txid pk sig)
                else Nothing
        in case asum sigMatches of
            Just m  -> pure m
            Nothing -> throwError $ SignatureMissing pkh
    _ -> throwError $ InOutTypeMismatch txin txo

-- | Check that a matching pair of transaction input and transaction output is
--   valid. If this is a pay-to-script output then the script hash needs to be
--   correct and script evaluation has to terminate successfully. If this is a
--   pay-to-pubkey output then the signature needs to match the public key that
--   locks it.
checkMatch :: ValidationMonad m => TxInfo -> InOutMatch -> m ()
checkMatch txinfo = \case
    ScriptMatch txOutRef vl r d -> do
        let
            ptx' = ScriptContext { scriptContextTxInfo = txinfo, scriptContextPurpose = Spending txOutRef }
            vd = Context (toBuiltinData ptx')
        case runExcept $ runScript vd vl d r of
            Left e -> do
                tell [validatorScriptValidationEvent vd vl d r (Left e)]
                throwError $ ScriptFailure e
            res -> tell [validatorScriptValidationEvent vd vl d r res]
    PubKeyMatch msg pk sig -> unless (signedBy sig pk msg) $ throwError $ InvalidSignature pk sig

-- | Check if the value produced by a transaction equals the value consumed by it.
checkValuePreserved :: ValidationMonad m => Tx -> m ()
checkValuePreserved t = do
    inVal <- (P.+) (txMint t) <$> fmap fold (traverse (lkpValue . txInputRef) (view inputs t))
    let outVal = txFee t P.+ foldMap txOutValue (txOutputs t)
    if outVal == inVal
    then pure ()
    else throwError $ ValueNotPreserved inVal outVal

-- | Check if all values produced and consumed by a transaction are non-negative.
checkPositiveValues :: ValidationMonad m => Tx -> m ()
checkPositiveValues t =
    if validValuesTx t
    then pure ()
    else throwError $ NegativeValue t

{-# INLINABLE minAdaTxOut #-}
-- Minimum required Ada for each tx output.
--
-- TODO: Should be removed.
minAdaTxOut :: Ada
minAdaTxOut = Ada.lovelaceOf minTxOut

{-# INLINABLE minTxOut #-}
minTxOut :: Integer
minTxOut = 2_000_000

-- Minimum required Lovelace for each tx output.
--
minLovelaceTxOut :: Lovelace
minLovelaceTxOut = Lovelace minTxOut

-- | Check if each transaction outputs produced a minimum lovelace output.
checkMinAdaInTxOutputs :: ValidationMonad m => Tx -> m ()
checkMinAdaInTxOutputs t@Tx { txOutputs } = do
    params <- vctxParams <$> ask
    for_ txOutputs $ \txOut -> do
        let
            minAdaTxOut' = fromRight minAdaTxOut $
                fromPlutusTxOutUnsafe params txOut <&> \txOut' -> evaluateMinLovelaceOutput params txOut'
        if Ada.fromValue (txOutValue txOut) >= minAdaTxOut'
            then pure ()
            else throwError $ ValueContainsLessThanMinAda t txOut (Ada.toValue minAdaTxOut')

-- | Check if the fees are paid exclusively in Ada.
checkFeeIsAda :: ValidationMonad m => Tx -> m ()
checkFeeIsAda t =
    if (Ada.toValue $ Ada.fromValue $ txFee t) == txFee t
    then pure ()
    else throwError $ NonAdaFees t

-- | Minimum transaction fee.
minFee :: Tx -> V.Value
minFee = const (Ada.lovelaceValueOf 10)

-- | TODO Should be calculated based on the maximum script size permitted on
-- the Cardano blockchain.
maxFee :: Ada
maxFee = Ada.lovelaceOf 1_000_000

-- | Check that transaction fee is bigger than the minimum fee.
--   Skip the check on the first transaction (no inputs).
checkTransactionFee :: ValidationMonad m => Tx -> m ()
checkTransactionFee tx =
    if minFee tx `V.leq` txFee tx
    then pure ()
    else throwError $ TransactionFeeTooLow (txFee tx) (minFee tx)

-- | Create the data about the transaction which will be passed to a validator script.
mkTxInfo :: ValidationMonad m => Tx -> m TxInfo
mkTxInfo tx = do
    slotCfg <- pSlotConfig . vctxParams <$> ask
    txins <- traverse mkIn $ view inputs tx
    let ptx = TxInfo
            { txInfoInputs = txins
            , txInfoOutputs = txOutputs tx
            , txInfoMint = txMint tx
            , txInfoFee = txFee tx
            , txInfoDCert = [] -- DCerts not supported in emulator
            , txInfoWdrl = [] -- Withdrawals not supported in emulator
            , txInfoValidRange = TimeSlot.slotRangeToPOSIXTimeRange slotCfg $ txValidRange tx
            , txInfoSignatories = fmap pubKeyHash $ Map.keys (tx ^. signatures)
            , txInfoData = Map.toList (tx ^. datumWitnesses)
            , txInfoId = txId tx
            }
    pure ptx

-- | Create the data about a transaction input which will be passed to a validator script.
mkIn :: ValidationMonad m => TxInput -> m Validation.TxInInfo
mkIn TxInput{txInputRef} = do
    txOut <- lkpTxOut txInputRef
    pure $ Validation.TxInInfo{Validation.txInInfoOutRef = txInputRef, Validation.txInInfoResolved=txOut}

data ScriptType = ValidatorScript Validator Datum | MintingPolicyScript MintingPolicy
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | A script (MPS or validator) that was run during transaction validation
data ScriptValidationEvent =
    ScriptValidationEvent
        { sveScript   :: Script -- ^ The script applied to all arguments
        , sveResult   :: Either ScriptError (Api.ExBudget, [Text]) -- ^ Result of running the script: an error or the 'ExBudget' and trace logs
        , sveRedeemer :: Redeemer
        , sveType     :: ScriptType -- ^ What type of script it was
        }
    | ScriptValidationResultOnlyEvent
        { sveResult   :: Either ScriptError (Api.ExBudget, [Text])
        }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

validatorScriptValidationEvent
    :: Context
    -> Validator
    -> Datum
    -> Redeemer
    -> Either ScriptError (Api.ExBudget, [Text])
    -> ScriptValidationEvent
validatorScriptValidationEvent ctx validator datum redeemer result =
    ScriptValidationEvent
        { sveScript = applyValidator ctx validator datum redeemer
        , sveResult = result
        , sveRedeemer = redeemer
        , sveType = ValidatorScript validator datum
        }

mpsValidationEvent
    :: Context
    -> MintingPolicy
    -> Redeemer
    -> Either ScriptError (Api.ExBudget, [Text])
    -> ScriptValidationEvent
mpsValidationEvent ctx mps red result =
    ScriptValidationEvent
        { sveScript = applyMintingPolicyScript ctx mps red
        , sveResult = result
        , sveRedeemer = red
        , sveType = MintingPolicyScript mps
        }

data ValidatorMode = FullyAppliedValidators | UnappliedValidators
    deriving (Eq, Ord, Show)

-- | Get the script from a @ScriptValidationEvent@ in either fully applied or unapplied form.
getScript :: ValidatorMode -> ScriptValidationEvent -> Script
getScript FullyAppliedValidators ScriptValidationEvent{sveScript} = sveScript
getScript UnappliedValidators ScriptValidationEvent{sveType} =
    case sveType of
        ValidatorScript (Validator script) _    -> script
        MintingPolicyScript (MintingPolicy mps) -> mps
getScript _ ScriptValidationResultOnlyEvent{} = error "getScript: unexpected ScriptValidationResultOnlyEvent"

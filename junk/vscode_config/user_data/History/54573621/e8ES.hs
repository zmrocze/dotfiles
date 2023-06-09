{-# OPTIONS_GHC -Wno-orphans    #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE OverloadedLists    #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE ViewPatterns       #-}

{-|

Interface to the transaction types from 'cardano-api'

-}
module Ledger.Tx.CardanoAPI(
  CardanoBuildTx(..)
  , SomeCardanoApiTx(..)
  , txOutRefs
  , unspentOutputsTx
  , fromCardanoTxId
  , fromCardanoTxIn
  , fromCardanoTxInsCollateral
  , fromCardanoTxOut
  , fromCardanoTxOutDatumHash
  , fromCardanoTxOutDatum
  , fromCardanoAddressInEra
  , fromCardanoAddress
  , fromCardanoMintValue
  , fromCardanoValue
  , fromCardanoPolicyId
  , fromCardanoFee
  , fromCardanoValidityRange
  , fromCardanoScriptInEra
  , fromCardanoPaymentKeyHash
  , fromCardanoScriptData
  , fromCardanoPlutusScript
  , fromCardanoScriptInAnyLang
  , fromTxScriptValidity
  , toTxScriptValidity
  , scriptDataFromCardanoTxBody
  , plutusScriptsFromTxBody
  , makeTransactionBody
  , toCardanoTxBody
  , toCardanoTxBodyContent
  , toCardanoTxIn
  , toCardanoTxInsCollateral
  , toCardanoTxInWitness
  , toCardanoTxOut
  , toCardanoTxOutUnsafe
  , toCardanoTxOutDatumHash
  , toCardanoTxOutValue
  , toCardanoAddressInEra
  , toCardanoMintValue
  , toCardanoValue
  , toCardanoFee
  , toCardanoValidityRange
  , toCardanoScriptInEra
  , toCardanoPaymentKeyHash
  , toCardanoScriptData
  , toCardanoScriptDataHash
  , toCardanoScriptHash
  , toCardanoPlutusScript
  , toCardanoScriptInAnyLang
  , toCardanoTxId
  , ToCardanoError(..)
  , FromCardanoError(..)
  , deserialiseFromRawBytes
  , zeroExecutionUnits
) where

import Cardano.Api qualified as C
import Cardano.Api.Byron qualified as C
import Cardano.Api.Shelley qualified as C
import Cardano.BM.Data.Tracer (ToObject)
import Cardano.Chain.Common (addrToBase58)
import Cardano.Ledger.Alonzo.Language qualified as Alonzo
import Cardano.Ledger.Alonzo.Scripts qualified as Alonzo
import Cardano.Ledger.Alonzo.TxWitness qualified as Alonzo
import Cardano.Ledger.Core qualified as Ledger
import Codec.Serialise (Serialise, deserialiseOrFail)
import Codec.Serialise qualified as Codec
import Codec.Serialise.Decoding (Decoder, decodeBytes, decodeSimple)
import Codec.Serialise.Encoding (Encoding (Encoding), Tokens (TkBytes, TkSimple))
import Control.Applicative ((<|>))
import Control.Lens ((&), (.~), (?~))
import Control.Monad (when)
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), object, (.:), (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types (Parser, parseFail, prependFailure, typeMismatch)
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.ByteString.Short qualified as SBS
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.OpenApi (NamedSchema (NamedSchema), OpenApiType (OpenApiObject), byteSchema, declareSchemaRef, properties,
                     required, sketchSchema, type_)
import Data.OpenApi qualified as OpenApi
import Data.Proxy (Proxy (Proxy))
import Data.Tuple (swap)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Ledger.Ada qualified as Ada
import Ledger.Address qualified as L
import Ledger.Params qualified as L
import Ledger.Scripts qualified as L
import Ledger.Slot qualified as L
import Ledger.Tx.CardanoAPITemp (makeTransactionBody')
import Ledger.Tx.Internal qualified as L
import Plutus.Script.Utils.Scripts (datumHash)
import Plutus.Script.Utils.V1.Scripts qualified as PV1
import Plutus.Script.Utils.V2.Scripts qualified as PV2
import Plutus.V1.Ledger.Api qualified as PV1
import Plutus.V1.Ledger.Credential qualified as Credential
import Plutus.V1.Ledger.Tx qualified as PV1
import Plutus.V1.Ledger.Value qualified as Value
import Plutus.V2.Ledger.Api qualified as PV2
import PlutusTx.Prelude qualified as PlutusTx
import Prettyprinter (Pretty (pretty), colon, viaShow, (<+>))

newtype CardanoBuildTx = CardanoBuildTx { getCardanoBuildTx :: C.TxBodyContent C.BuildTx C.BabbageEra }
  deriving (Eq, Show)

instance ToJSON CardanoBuildTx where
  toJSON = error "TODO: ToJSON CardanoBuildTx"

instance FromJSON CardanoBuildTx where
  parseJSON _ = parseFail "TODO: FromJSON CardanoBuildTx"

instance OpenApi.ToSchema CardanoBuildTx where
  -- TODO: implement the schema
  declareNamedSchema _ = return $ NamedSchema (Just "CardanoBuildTx") mempty

instance (Typeable era, Typeable mode) => OpenApi.ToSchema (C.EraInMode era mode) where
  declareNamedSchema _ = do
    return $ NamedSchema (Just "EraInMode") $ sketchSchema C.BabbageEraInCardanoMode

instance (Typeable era) => OpenApi.ToSchema (C.Tx era) where
  declareNamedSchema _ = do
    return $ NamedSchema (Just "Tx") byteSchema

-- | Cardano tx from any era.
data SomeCardanoApiTx where
  SomeTx :: C.IsCardanoEra era => C.Tx era -> C.EraInMode era C.CardanoMode -> SomeCardanoApiTx

instance Eq SomeCardanoApiTx where
  (SomeTx tx1 C.ByronEraInCardanoMode) == (SomeTx tx2 C.ByronEraInCardanoMode)     = tx1 == tx2
  (SomeTx tx1 C.ShelleyEraInCardanoMode) == (SomeTx tx2 C.ShelleyEraInCardanoMode) = tx1 == tx2
  (SomeTx tx1 C.AllegraEraInCardanoMode) == (SomeTx tx2 C.AllegraEraInCardanoMode) = tx1 == tx2
  (SomeTx tx1 C.MaryEraInCardanoMode) == (SomeTx tx2 C.MaryEraInCardanoMode)       = tx1 == tx2
  (SomeTx tx1 C.AlonzoEraInCardanoMode) == (SomeTx tx2 C.AlonzoEraInCardanoMode)   = tx1 == tx2
  (SomeTx tx1 C.BabbageEraInCardanoMode) == (SomeTx tx2 C.BabbageEraInCardanoMode) = tx1 == tx2
  _ == _                                                                           = False

deriving instance Show SomeCardanoApiTx

instance Serialise SomeCardanoApiTx where
  encode (SomeTx tx eraInMode) = encodedMode eraInMode <> Encoding (TkBytes (C.serialiseToCBOR tx))
    where
      encodedMode :: C.EraInMode era C.CardanoMode -> Encoding
      -- 0 and 1 are for ByronEraInByronMode and ShelleyEraInShelleyMode
      encodedMode C.ByronEraInCardanoMode   = Encoding (TkSimple 2)
      encodedMode C.ShelleyEraInCardanoMode = Encoding (TkSimple 3)
      encodedMode C.AllegraEraInCardanoMode = Encoding (TkSimple 4)
      encodedMode C.MaryEraInCardanoMode    = Encoding (TkSimple 5)
      encodedMode C.AlonzoEraInCardanoMode  = Encoding (TkSimple 6)
      encodedMode C.BabbageEraInCardanoMode = Encoding (TkSimple 7)
  decode = do
    w <- decodeSimple
    case w of
      2 -> decodeTx C.AsByronEra C.ByronEraInCardanoMode
      3 -> decodeTx C.AsShelleyEra C.ShelleyEraInCardanoMode
      4 -> decodeTx C.AsAllegraEra C.AllegraEraInCardanoMode
      5 -> decodeTx C.AsMaryEra C.MaryEraInCardanoMode
      6 -> decodeTx C.AsAlonzoEra C.AlonzoEraInCardanoMode
      7 -> decodeTx C.AsBabbageEra C.BabbageEraInCardanoMode
      _ -> fail "Unexpected value while decoding Cardano.Api.EraInMode"
    where
      decodeTx :: C.IsCardanoEra era => C.AsType era -> C.EraInMode era C.CardanoMode -> Decoder s SomeCardanoApiTx
      decodeTx asType eraInMode = do
        bytes <- decodeBytes
        tx <- either (const $ fail "Failed to decode Cardano.Api.Tx") pure $ C.deserialiseFromCBOR (C.AsTx asType) bytes
        pure $ SomeTx tx eraInMode

instance ToJSON SomeCardanoApiTx where
  toJSON (SomeTx tx eraInMode) =
    object [ "tx" .= C.serialiseToTextEnvelope Nothing tx
           , "eraInMode" .= eraInMode
           ]

-- | Converting 'SomeCardanoApiTx' to JSON.
--
-- If the "tx" field is from an unknown era, the JSON parser will print an
-- error at runtime while parsing.
instance FromJSON SomeCardanoApiTx where
  parseJSON v = parseByronInCardanoModeTx v
            <|> parseShelleyEraInCardanoModeTx v
            <|> parseAllegraEraInCardanoModeTx v
            <|> parseMaryEraInCardanoModeTx v
            <|> parseAlonzoEraInCardanoModeTx v
            <|> parseBabbageEraInCardanoModeTx v
            <|> parseEraInCardanoModeFail v

parseByronInCardanoModeTx :: Aeson.Value -> Parser SomeCardanoApiTx
parseByronInCardanoModeTx =
  parseSomeCardanoTx "Failed to parse ByronEra 'tx' field from SomeCardanoApiTx"
                     (C.AsTx C.AsByronEra)

parseShelleyEraInCardanoModeTx :: Aeson.Value -> Parser SomeCardanoApiTx
parseShelleyEraInCardanoModeTx =
  parseSomeCardanoTx "Failed to parse ShelleyEra 'tx' field from SomeCardanoApiTx"
                     (C.AsTx C.AsShelleyEra)

parseMaryEraInCardanoModeTx :: Aeson.Value -> Parser SomeCardanoApiTx
parseMaryEraInCardanoModeTx =
  parseSomeCardanoTx "Failed to parse MaryEra 'tx' field from SomeCardanoApiTx"
                     (C.AsTx C.AsMaryEra)

parseAllegraEraInCardanoModeTx :: Aeson.Value -> Parser SomeCardanoApiTx
parseAllegraEraInCardanoModeTx =
  parseSomeCardanoTx "Failed to parse AllegraEra 'tx' field from SomeCardanoApiTx"
                     (C.AsTx C.AsAllegraEra)

parseAlonzoEraInCardanoModeTx :: Aeson.Value -> Parser SomeCardanoApiTx
parseAlonzoEraInCardanoModeTx =
  parseSomeCardanoTx "Failed to parse AlonzoEra 'tx' field from SomeCardanoApiTx"
                     (C.AsTx C.AsAlonzoEra)

-- TODO Uncomment the implementation once Cardano.Api adds a FromJSON instance
-- for 'EraInMode BabbageEra CardanoMode':
-- https://github.com/input-output-hk/cardano-node/pull/3837
parseBabbageEraInCardanoModeTx :: Aeson.Value -> Parser SomeCardanoApiTx
parseBabbageEraInCardanoModeTx (Aeson.Object v) =
    SomeTx
    <$> (v .: "tx" >>= \envelope -> either (const $ parseFail "Failed to parse BabbageEra 'tx' field from SomeCardanoApiTx")
                                           pure
                                           $ C.deserialiseFromTextEnvelope (C.AsTx C.AsBabbageEra) envelope)
    <*> pure C.BabbageEraInCardanoMode -- This is a workaround that only works because we tried all other eras first
parseBabbageEraInCardanoModeTx invalid =
  prependFailure "parsing SomeCardanoApiTx failed, "
      (typeMismatch "Object" invalid)
  -- parseSomeCardanoTx "Failed to parse BabbageEra 'tx' field from SomeCardanoApiTx"
  --                    (C.AsTx C.AsBabbageEra)

parseEraInCardanoModeFail :: Aeson.Value -> Parser SomeCardanoApiTx
parseEraInCardanoModeFail _ = fail "Unable to parse 'eraInMode'"

parseSomeCardanoTx
  :: ( FromJSON (C.EraInMode era C.CardanoMode)
     , C.IsCardanoEra era
     )
  => String
  -> C.AsType (C.Tx era)
  -> Aeson.Value
  -> Parser SomeCardanoApiTx
parseSomeCardanoTx errorMsg txAsType (Aeson.Object v) =
  SomeTx
    <$> (v .: "tx" >>= \envelope -> either (const $ parseFail errorMsg)
                                           pure
                                           $ C.deserialiseFromTextEnvelope txAsType envelope)
    <*> v .: "eraInMode"
parseSomeCardanoTx _ _ invalid =
    prependFailure "parsing SomeCardanoApiTx failed, "
      (typeMismatch "Object" invalid)

instance OpenApi.ToSchema SomeCardanoApiTx where
  declareNamedSchema _ = do
    txSchema <- declareSchemaRef (Proxy :: Proxy (C.Tx C.BabbageEra))
    eraInModeSchema <- declareSchemaRef (Proxy :: Proxy (C.EraInMode C.BabbageEra C.CardanoMode))
    return $ NamedSchema (Just "SomeCardanoApiTx") $ mempty
      & type_ ?~ OpenApiObject
      & properties .~
          [ ("tx", txSchema)
          , ("eraInMode", eraInModeSchema)
          ]
      & required .~ [ "tx", "eraInMode" ]

txOutRefs :: SomeCardanoApiTx -> [(PV1.TxOut, PV1.TxOutRef)]
txOutRefs (SomeTx (C.Tx txBody@(C.TxBody C.TxBodyContent{..}) _) _) =
  mkOut <$> zip [0..] plutusTxOuts
  where
    mkOut (i, o) = (o, PV1.TxOutRef (fromCardanoTxId $ C.getTxId txBody) i)
    plutusTxOuts = mapMaybe (either (const Nothing) Just . fromCardanoTxOut) txOuts

unspentOutputsTx :: SomeCardanoApiTx -> Map PV1.TxOutRef PV1.TxOut
unspentOutputsTx tx = Map.fromList $ swap <$> txOutRefs tx

-- | Given a 'C.TxScriptValidity era', if the @era@ supports scripts, return a
-- @True@ or @False@ depending on script validity. If the @era@ does not support
-- scripts, always return @True@.
fromTxScriptValidity :: C.TxScriptValidity era -> Bool
fromTxScriptValidity (C.TxScriptValidity C.TxScriptValiditySupportedInAlonzoEra C.ScriptValid)    = True
fromTxScriptValidity (C.TxScriptValidity C.TxScriptValiditySupportedInAlonzoEra C.ScriptInvalid)  = False
fromTxScriptValidity (C.TxScriptValidity C.TxScriptValiditySupportedInBabbageEra C.ScriptValid)   = True
fromTxScriptValidity (C.TxScriptValidity C.TxScriptValiditySupportedInBabbageEra C.ScriptInvalid) = False
fromTxScriptValidity C.TxScriptValidityNone                                                       = True

toTxScriptValidity :: C.ShelleyBasedEra era -> Bool -> C.TxScriptValidity era
toTxScriptValidity C.ShelleyBasedEraAlonzo True  = C.TxScriptValidity C.TxScriptValiditySupportedInAlonzoEra C.ScriptValid
toTxScriptValidity C.ShelleyBasedEraAlonzo False = C.TxScriptValidity C.TxScriptValiditySupportedInAlonzoEra C.ScriptInvalid
toTxScriptValidity C.ShelleyBasedEraBabbage True  = C.TxScriptValidity C.TxScriptValiditySupportedInBabbageEra C.ScriptValid
toTxScriptValidity C.ShelleyBasedEraBabbage False = C.TxScriptValidity C.TxScriptValiditySupportedInBabbageEra C.ScriptInvalid
toTxScriptValidity _ _ = C.TxScriptValidityNone

-- | Given a 'C.TxBody from a 'C.Tx era', return the datums and redeemers along
-- with their hashes.
scriptDataFromCardanoTxBody
  :: C.TxBody era
  -> (Map L.DatumHash L.Datum, PV1.Redeemers)
scriptDataFromCardanoTxBody C.ByronTxBody {} = (mempty, mempty)
scriptDataFromCardanoTxBody (C.ShelleyTxBody _ _ _ C.TxBodyNoScriptData _ _) =
  (mempty, mempty)
scriptDataFromCardanoTxBody
  (C.ShelleyTxBody _ _ _ (C.TxBodyScriptData _ (Alonzo.TxDats' dats) (Alonzo.Redeemers' reds)) _ _) =

  let datums = Map.fromList
             $ fmap ( (\d -> (datumHash d, d))
                    . L.Datum
                    . fromCardanoScriptData
                    . C.fromAlonzoData
                    )
             $ Map.elems dats
      redeemers = Map.fromList
                $ map (\(ptr, rdmr) ->
                        ( redeemerPtrFromCardanoRdmrPtr ptr
                        , L.Redeemer
                         $ fromCardanoScriptData
                         $ C.fromAlonzoData
                         $ fst rdmr
                        )
                      )
                $ Map.toList reds
   in (datums, redeemers)

redeemerPtrFromCardanoRdmrPtr :: Alonzo.RdmrPtr -> PV1.RedeemerPtr
redeemerPtrFromCardanoRdmrPtr (Alonzo.RdmrPtr rdmrTag ptr) = PV1.RedeemerPtr t (toInteger ptr)
  where
    t = case rdmrTag of
      Alonzo.Spend -> PV1.Spend
      Alonzo.Mint  -> PV1.Mint
      Alonzo.Cert  -> PV1.Cert
      Alonzo.Rewrd -> PV1.Reward

-- | Extract plutus scripts from a Cardano API tx body.
--
-- Note that Plutus scripts are only supported in Alonzo era and onwards.
plutusScriptsFromTxBody :: C.TxBody era -> Map P.ScriptHash (P.Versioned P.Script)
plutusScriptsFromTxBody C.ByronTxBody {} = mempty
plutusScriptsFromTxBody (C.ShelleyTxBody shelleyBasedEra _ scripts _ _ _) =
  Map.fromList $ mapMaybe (fromLedgerScript shelleyBasedEra) scripts

-- | Convert a script from a Cardano api in shelley based era to a Plutus script along with it's hash.
--
-- Note that Plutus scripts are only supported in Alonzo era and onwards.
fromLedgerScript
  :: C.ShelleyBasedEra era
  -> Ledger.Script (C.ShelleyLedgerEra era)
  -> Maybe (P.ScriptHash, P.Versioned P.Script)
fromLedgerScript C.ShelleyBasedEraShelley _      = Nothing
fromLedgerScript C.ShelleyBasedEraAllegra _      = Nothing
fromLedgerScript C.ShelleyBasedEraMary _         = Nothing
fromLedgerScript C.ShelleyBasedEraAlonzo script  = fromLedgerPlutusScript script
fromLedgerScript C.ShelleyBasedEraBabbage script = fromLedgerPlutusScript script

-- | Convert a `cardano-ledger` Plutus script from the Alonzo era and onwards to
-- a 'Script' along with it's hash.
fromLedgerPlutusScript :: Alonzo.Script a -> Maybe (P.ScriptHash, P.Versioned P.Script)
fromLedgerPlutusScript Alonzo.TimelockScript {} = Nothing
fromLedgerPlutusScript (Alonzo.PlutusScript Alonzo.PlutusV1 bs) =
  let script = fmap (\s -> (PV1.scriptHash s, P.Versioned s P.PlutusV1))
             $ deserialiseOrFail
             $ BSL.fromStrict
             $ SBS.fromShort bs
   in either (const Nothing) Just script
fromLedgerPlutusScript (Alonzo.PlutusScript Alonzo.PlutusV2 bs) =
  let script = fmap (\s -> (PV2.scriptHash s, P.Versioned s P.PlutusV2))
             $ deserialiseOrFail
             $ BSL.fromStrict
             $ SBS.fromShort bs
   in either (const Nothing) Just script

toCardanoTxBodyContent
    :: L.Params
    -> [L.PaymentPubKeyHash] -- ^ Required signers of the transaction
    -> L.Tx
    -> Either ToCardanoError CardanoBuildTx
toCardanoTxBodyContent L.Params{L.pProtocolParams, L.pNetworkId} sigs tx@L.Tx{..} = do
    -- TODO: translate all fields
    txIns <- traverse (toCardanoTxInBuild tx) txInputs
    txInsReference <- traverse (toCardanoTxIn . L.txInputRef) txReferenceInputs
    txInsCollateral <- toCardanoTxInsCollateral txCollateral
    txOuts <- traverse (toCardanoTxOut pNetworkId (lookupDatum txData)) txOutputs
    txFee' <- toCardanoFee txFee
    txValidityRange <- toCardanoValidityRange txValidRange
    txMintValue <- toCardanoMintValue tx
    txExtraKeyWits <- C.TxExtraKeyWitnesses C.ExtraKeyWitnessesInBabbageEra <$> traverse toCardanoPaymentKeyHash sigs
    withdrawals <- toWithdrawals txScripts pNetworkId txWithdrawals
    pure $ CardanoBuildTx $ C.TxBodyContent
        { txIns = txIns
        , txInsReference = C.TxInsReference C.ReferenceTxInsScriptsInlineDatumsInBabbageEra txInsReference
        , txInsCollateral = txInsCollateral
        , txOuts = txOuts
        , txTotalCollateral = C.TxTotalCollateralNone -- TODO Change when going to Babbage era txs
        , txReturnCollateral = C.TxReturnCollateralNone -- TODO Change when going to Babbage era txs
        , txFee = txFee'
        , txValidityRange = txValidityRange
        , txMintValue = txMintValue
        , txProtocolParams = C.BuildTxWith $ Just pProtocolParams
        , txScriptValidity = C.TxScriptValidityNone
        , txExtraKeyWits
        -- unused:
        , txMetadata = C.TxMetadataNone
        , txAuxScripts = C.TxAuxScriptsNone
        , txWithdrawals = withdrawals
        , txCertificates = C.TxCertificatesNone
        , txUpdateProposal = C.TxUpdateProposalNone
        }

-- TODO: Support V2.
toWithdrawals :: Map L.ScriptHash PV1.Script
  -> C.NetworkId
  -> [L.Withdrawal]
  -> Either ToCardanoError (C.TxWithdrawals C.BuildTx C.BabbageEra)
toWithdrawals txScripts networkId = \case
  [] -> pure C.TxWithdrawalsNone
  xs -> C.TxWithdrawals C.WithdrawalsInBabbageEra <$> mapM toWithdraw xs

  where
    toWithdraw L.Withdrawal{withdrawalCredential, withdrawalAmount, withdrawalRedeemer} = do
      saddr <- toCardanoStakeAddress networkId withdrawalCredential
      witness <- toStakeWitness withdrawalRedeemer withdrawalCredential
      pure (saddr, C.Lovelace withdrawalAmount, witness)

    toStakeWitness withdrawalRedeemer cred = case cred of
      PV1.PubKeyCredential _pkh -> pure $ C.BuildTxWith $ C.KeyWitness C.KeyWitnessForStakeAddr
      PV1.ScriptCredential _vh -> case (,) <$> withdrawalRedeemer <*> L.lookupValidator txScripts _vh of
        -- Hardcoded V1.
        Just (redeemer, PV1.Validator script) -> C.BuildTxWith . C.ScriptWitness C.ScriptWitnessForStakeAddr <$> toCardanoScriptWitness L.PlutusV1 C.NoScriptDatumForStake redeemer script
        Nothing                    -> Left MissingStakeValidator

toCardanoStakeAddress :: C.NetworkId -> PV1.Credential -> Either ToCardanoError C.StakeAddress
toCardanoStakeAddress networkId credential =
  C.StakeAddress (C.toShelleyNetwork networkId) . C.toShelleyStakeCredential <$> toCardanoStakingCredential credential

toCardanoStakingCredential :: PV1.Credential -> Either ToCardanoError C.StakeCredential
toCardanoStakingCredential (PV1.PubKeyCredential pubKeyHash) = C.StakeCredentialByKey <$> toCardanoStakeKeyHash pubKeyHash
toCardanoStakingCredential (PV1.ScriptCredential validatorHash) = C.StakeCredentialByScript <$> toCardanoScriptHash validatorHash


toCardanoTxBody ::
    L.Params -- ^ Parameters to use.
    -> [L.PaymentPubKeyHash] -- ^ Required signers of the transaction
    -> L.Tx
    -> Either ToCardanoError (C.TxBody C.BabbageEra)
toCardanoTxBody params sigs tx = do
    txBodyContent <- toCardanoTxBodyContent params sigs tx
    makeTransactionBody mempty txBodyContent

makeTransactionBody
    :: Map Alonzo.RdmrPtr Alonzo.ExUnits
    -> CardanoBuildTx
    -> Either ToCardanoError (C.TxBody C.BabbageEra)
makeTransactionBody exUnits (CardanoBuildTx txBodyContent) =
  first (TxBodyError . C.displayError) $ makeTransactionBody' exUnits txBodyContent

fromCardanoTxIn :: C.TxIn -> PV1.TxOutRef
fromCardanoTxIn (C.TxIn txId (C.TxIx txIx)) = PV1.TxOutRef (fromCardanoTxId txId) (toInteger txIx)

toCardanoTxInBuild :: L.Tx -> L.TxInput -> Either ToCardanoError (C.TxIn, C.BuildTxWith C.BuildTx (C.Witness C.WitCtxTxIn C.BabbageEra))
toCardanoTxInBuild tx (L.TxInput txInRef txInType) = (,) <$> toCardanoTxIn txInRef <*> (C.BuildTxWith <$> toCardanoTxInWitness tx txInType)

toCardanoTxIn :: PV1.TxOutRef -> Either ToCardanoError C.TxIn
toCardanoTxIn (PV1.TxOutRef txId txIx) = C.TxIn <$> toCardanoTxId txId <*> pure (C.TxIx (fromInteger txIx))

fromCardanoTxId :: C.TxId -> PV1.TxId
fromCardanoTxId txId = PV1.TxId $ PlutusTx.toBuiltin $ C.serialiseToRawBytes txId

toCardanoTxId :: PV1.TxId -> Either ToCardanoError C.TxId
toCardanoTxId (PV1.TxId bs) =
    tag "toCardanoTxId"
    $ deserialiseFromRawBytes C.AsTxId $ PlutusTx.fromBuiltin bs

fromCardanoTxInsCollateral :: C.TxInsCollateral era -> [L.TxIn]
fromCardanoTxInsCollateral C.TxInsCollateralNone       = []
fromCardanoTxInsCollateral (C.TxInsCollateral _ txIns) = map (L.pubKeyTxIn . fromCardanoTxIn) txIns

toCardanoTxInsCollateral :: [L.TxInput] -> Either ToCardanoError (C.TxInsCollateral C.BabbageEra)
toCardanoTxInsCollateral = fmap (C.TxInsCollateral C.CollateralInBabbageEra) . traverse (toCardanoTxIn . L.txInputRef)

<<<<<<< HEAD
toCardanoTxInWitness :: L.Tx -> L.TxInputType -> Either ToCardanoError (C.Witness C.WitCtxTxIn C.BabbageEra)
toCardanoTxInWitness _ L.TxConsumePublicKeyAddress = pure (C.KeyWitness C.KeyWitnessForSpending)
toCardanoTxInWitness _ L.TxConsumeSimpleScriptAddress = Left SimpleScriptsNotSupportedToCardano -- TODO: Better support for simple scripts
toCardanoTxInWitness tx
    (L.TxConsumeScriptAddress
        (L.Redeemer redeemer)
        valh
        dh)
    = do
      (PV1.Datum datum) <- maybe (Left MissingDatum) pure $ Map.lookup dh (L.txData tx)
      (L.Versioned (L.Validator validator) lang) <- maybe (Left MissingInputValidator) pure $ L.lookupValidator (L.txScripts tx) valh
      case lang of
        L.PlutusV1 -> 
      C.ScriptWitness C.ScriptWitnessForSpending <$>
=======
toCardanoTxInWitness :: P.TxInType -> Either ToCardanoError (C.Witness C.WitCtxTxIn C.BabbageEra)
toCardanoTxInWitness P.ConsumePublicKeyAddress = pure (C.KeyWitness C.KeyWitnessForSpending)
toCardanoTxInWitness P.ConsumeSimpleScriptAddress = Left SimpleScriptsNotSupportedToCardano -- TODO: Better support for simple scripts
toCardanoTxInWitness
    (P.ConsumeScriptAddress
        (P.Versioned (P.Validator validator) P.PlutusV1)
        (P.Redeemer redeemer)
        (P.Datum datum))
    = C.ScriptWitness C.ScriptWitnessForSpending <$>
>>>>>>> next-node
        (C.PlutusScriptWitness C.PlutusScriptV1InBabbage C.PlutusScriptV1
        <$> fmap C.PScript (toCardanoPlutusScript (C.AsPlutusScript C.AsPlutusScriptV1) validator)
        <*> pure (C.ScriptDatumForTxIn $ toCardanoScriptData datum)
        <*> pure (toCardanoScriptData redeemer)
        <*> pure zeroExecutionUnits
        )
<<<<<<< HEAD
toCardanoTxInWitness tx
    (L.TxConsumeScriptAddress
        L.PlutusV2
        (L.Redeemer redeemer)
        vh
        dh)
    = do
      (PV1.Datum datum) <- maybe (Left MissingDatum) pure $ Map.lookup dh (L.txData tx)
      (PV1.Validator validator) <- maybe (Left MissingInputValidator) pure $ L.lookupValidator (L.txScripts tx) vh
      C.ScriptWitness C.ScriptWitnessForSpending <$>
=======
toCardanoTxInWitness
    (P.ConsumeScriptAddress
        (P.Versioned (P.Validator validator) P.PlutusV2)
        (P.Redeemer redeemer)
        (P.Datum datum))
    = C.ScriptWitness C.ScriptWitnessForSpending <$>
>>>>>>> next-node
        (C.PlutusScriptWitness C.PlutusScriptV2InBabbage C.PlutusScriptV2
        <$> fmap C.PScript (toCardanoPlutusScript (C.AsPlutusScript C.AsPlutusScriptV2) validator)
        <*> pure (C.ScriptDatumForTxIn $ toCardanoScriptData datum)
        <*> pure (toCardanoScriptData redeemer)
        <*> pure zeroExecutionUnits
        )

<<<<<<< HEAD
=======
toCardanoMintWitness :: PV1.Redeemers -> Int -> P.Versioned P.MintingPolicy -> Either ToCardanoError (C.ScriptWitness C.WitCtxMint C.BabbageEra)
toCardanoMintWitness redeemers idx (P.Versioned (P.MintingPolicy script) lang) = do
    let redeemerPtr = PV1.RedeemerPtr PV1.Mint (fromIntegral idx)
    P.Redeemer redeemer <- maybe (Left MissingMintingPolicyRedeemer) Right (Map.lookup redeemerPtr redeemers)
    case lang of
      P.PlutusV1 ->
        C.PlutusScriptWitness C.PlutusScriptV1InBabbage C.PlutusScriptV1
            <$> fmap C.PScript (toCardanoPlutusScript (C.AsPlutusScript C.AsPlutusScriptV1) script)
            <*> pure C.NoScriptDatumForMint
            <*> pure (C.fromPlutusData $ PV1.toData redeemer)
            <*> pure zeroExecutionUnits
      P.PlutusV2 ->
        C.PlutusScriptWitness C.PlutusScriptV2InBabbage C.PlutusScriptV2
            <$> fmap C.PScript (toCardanoPlutusScript (C.AsPlutusScript C.AsPlutusScriptV2) script)
            <*> pure C.NoScriptDatumForMint
            <*> pure (C.fromPlutusData $ PV1.toData redeemer)
            <*> pure zeroExecutionUnits

>>>>>>> next-node

-- TODO: Support V2 minting scripts.
toCardanoMintWitness :: PV1.Redeemer -> Maybe PV1.MintingPolicy -> Either ToCardanoError (C.ScriptWitness C.WitCtxMint C.BabbageEra)
toCardanoMintWitness _ Nothing = Left MissingMintingPolicy
toCardanoMintWitness redeemer (Just (PV1.MintingPolicy script)) =
  -- Hardcoded V1
  toCardanoScriptWitness L.PlutusV1 C.NoScriptDatumForMint redeemer script

toCardanoScriptWitness :: PV1.ToData a =>
  L.LedgerPlutusVersion
  -> C.ScriptDatum witctx
  -> a
  -> PV1.Script
  -> Either ToCardanoError (C.ScriptWitness witctx C.BabbageEra)
toCardanoScriptWitness pv datum redeemer script = (case pv of
    L.PlutusV1 ->
      C.PlutusScriptWitness C.PlutusScriptV1InBabbage C.PlutusScriptV1
          <$> fmap C.PScript (toCardanoPlutusScript (C.AsPlutusScript C.AsPlutusScriptV1) script)
    L.PlutusV2 ->
      C.PlutusScriptWitness C.PlutusScriptV2InBabbage C.PlutusScriptV2
          <$> fmap C.PScript (toCardanoPlutusScript (C.AsPlutusScript C.AsPlutusScriptV2) script)
  ) <*> pure datum
    <*> pure (C.fromPlutusData $ PV1.toData redeemer)
    <*> pure zeroExecutionUnits

-- TODO Handle reference script once 'PV1.TxOut' supports it (or when we use
-- exclusively 'C.TxOut' in all the codebase).
fromCardanoTxOut :: C.TxOut C.CtxTx era -> Either FromCardanoError PV1.TxOut
fromCardanoTxOut (C.TxOut addr value dh _) =
    PV1.TxOut
    <$> fromCardanoAddressInEra addr
    <*> pure (fromCardanoTxOutValue value)
    <*> pure (fromCardanoTxOutDatumHash dh)

toCardanoTxOut
    :: C.NetworkId
    -> (Maybe L.DatumHash -> Either ToCardanoError (C.TxOutDatum ctx C.BabbageEra))
    -> PV1.TxOut
    -> Either ToCardanoError (C.TxOut ctx C.BabbageEra)
toCardanoTxOut networkId fromHash (PV1.TxOut addr value dh) =
    C.TxOut <$> toCardanoAddressInEra networkId addr
            <*> toCardanoTxOutValue value
            <*> fromHash dh
            <*> pure C.ReferenceScriptNone

toCardanoTxOutUnsafe
    :: C.NetworkId
    -> (Maybe L.DatumHash -> Either ToCardanoError (C.TxOutDatum ctx C.BabbageEra))
    -> PV1.TxOut
    -> Either ToCardanoError (C.TxOut ctx C.BabbageEra)
toCardanoTxOutUnsafe networkId fromHash (PV1.TxOut addr value dh) =
    C.TxOut <$> toCardanoAddressInEra networkId addr
            <*> toCardanoTxOutValueUnsafe value
            <*> fromHash dh
            <*> pure C.ReferenceScriptNone

lookupDatum :: Map L.DatumHash L.Datum -> Maybe L.DatumHash -> Either ToCardanoError (C.TxOutDatum C.CtxTx C.BabbageEra)
lookupDatum datums dh =
    case flip Map.lookup datums =<< dh of
        Just datum -> pure $ C.TxOutDatumInTx C.ScriptDataInBabbageEra (toCardanoScriptData $ L.getDatum datum)
        Nothing    -> toCardanoTxOutDatumHash dh

fromCardanoAddressInEra :: C.AddressInEra era -> Either FromCardanoError L.Address
fromCardanoAddressInEra (C.AddressInEra C.ByronAddressInAnyEra address) = fromCardanoAddress address
fromCardanoAddressInEra (C.AddressInEra _ address)                      = fromCardanoAddress address

fromCardanoAddress :: C.Address addrtype -> Either FromCardanoError L.Address
fromCardanoAddress (C.ByronAddress address) =
    Right $ L.Address plutusCredential Nothing
    where
      plutusCredential :: Credential.Credential
      plutusCredential =
          Credential.PubKeyCredential
        $ PV1.PubKeyHash
        $ PlutusTx.toBuiltin
        $ addrToBase58 address
fromCardanoAddress (C.ShelleyAddress _ paymentCredential stakeAddressReference) =
    L.Address (fromCardanoPaymentCredential (C.fromShelleyPaymentCredential paymentCredential))
        <$> fromCardanoStakeAddressReference (C.fromShelleyStakeReference stakeAddressReference)

toCardanoAddressInEra :: C.NetworkId -> L.Address -> Either ToCardanoError (C.AddressInEra C.BabbageEra)
toCardanoAddressInEra networkId (L.Address addressCredential addressStakingCredential) =
    C.AddressInEra (C.ShelleyAddressInEra C.ShelleyBasedEraBabbage) <$>
        (C.makeShelleyAddress networkId
            <$> toCardanoPaymentCredential addressCredential
            <*> toCardanoStakeAddressReference addressStakingCredential)

fromCardanoPaymentCredential :: C.PaymentCredential -> Credential.Credential
fromCardanoPaymentCredential (C.PaymentCredentialByKey paymentKeyHash) = Credential.PubKeyCredential (fromCardanoPaymentKeyHash paymentKeyHash)
fromCardanoPaymentCredential (C.PaymentCredentialByScript scriptHash) = Credential.ScriptCredential (fromCardanoScriptHash scriptHash)

toCardanoPaymentCredential :: Credential.Credential -> Either ToCardanoError C.PaymentCredential
toCardanoPaymentCredential (Credential.PubKeyCredential pubKeyHash) = C.PaymentCredentialByKey <$> toCardanoPaymentKeyHash (L.PaymentPubKeyHash pubKeyHash)
toCardanoPaymentCredential (Credential.ScriptCredential validatorHash) = C.PaymentCredentialByScript <$> toCardanoScriptHash validatorHash

fromCardanoPaymentKeyHash :: C.Hash C.PaymentKey -> PV1.PubKeyHash
fromCardanoPaymentKeyHash paymentKeyHash = PV1.PubKeyHash $ PlutusTx.toBuiltin $ C.serialiseToRawBytes paymentKeyHash

toCardanoPaymentKeyHash :: L.PaymentPubKeyHash -> Either ToCardanoError (C.Hash C.PaymentKey)
toCardanoPaymentKeyHash (L.PaymentPubKeyHash (PV1.PubKeyHash bs)) =
    let bsx = PlutusTx.fromBuiltin bs
        tg = "toCardanoPaymentKeyHash (" <> show (BS.length bsx) <> " bytes)"
    in tag tg $ deserialiseFromRawBytes (C.AsHash C.AsPaymentKey) bsx

fromCardanoScriptHash :: C.ScriptHash -> PV1.ValidatorHash
fromCardanoScriptHash scriptHash = PV1.ValidatorHash $ PlutusTx.toBuiltin $ C.serialiseToRawBytes scriptHash

toCardanoScriptHash :: PV1.ValidatorHash -> Either ToCardanoError C.ScriptHash
toCardanoScriptHash (PV1.ValidatorHash bs) = tag "toCardanoScriptHash" $ deserialiseFromRawBytes C.AsScriptHash $ PlutusTx.fromBuiltin bs

fromCardanoStakeAddressReference :: C.StakeAddressReference -> Either FromCardanoError (Maybe Credential.StakingCredential)
fromCardanoStakeAddressReference C.NoStakeAddress = pure Nothing
fromCardanoStakeAddressReference (C.StakeAddressByValue stakeCredential) =
    pure $ Just (Credential.StakingHash $ fromCardanoStakeCredential stakeCredential)
fromCardanoStakeAddressReference C.StakeAddressByPointer{} = pure Nothing

toCardanoStakeAddressReference :: Maybe Credential.StakingCredential -> Either ToCardanoError C.StakeAddressReference
toCardanoStakeAddressReference Nothing = pure C.NoStakeAddress
toCardanoStakeAddressReference (Just (Credential.StakingHash credential)) =
    C.StakeAddressByValue <$> toCardanoStakeCredential credential
toCardanoStakeAddressReference (Just Credential.StakingPtr{}) = Left StakingPointersNotSupported

fromCardanoStakeCredential :: C.StakeCredential -> Credential.Credential
fromCardanoStakeCredential (C.StakeCredentialByKey stakeKeyHash) = Credential.PubKeyCredential (fromCardanoStakeKeyHash stakeKeyHash)
fromCardanoStakeCredential (C.StakeCredentialByScript scriptHash) = Credential.ScriptCredential (fromCardanoScriptHash scriptHash)

toCardanoStakeCredential :: Credential.Credential -> Either ToCardanoError C.StakeCredential
toCardanoStakeCredential (Credential.PubKeyCredential pubKeyHash) = C.StakeCredentialByKey <$> toCardanoStakeKeyHash pubKeyHash
toCardanoStakeCredential (Credential.ScriptCredential validatorHash) = C.StakeCredentialByScript <$> toCardanoScriptHash validatorHash

fromCardanoStakeKeyHash :: C.Hash C.StakeKey -> PV1.PubKeyHash
fromCardanoStakeKeyHash stakeKeyHash = PV1.PubKeyHash $ PlutusTx.toBuiltin $ C.serialiseToRawBytes stakeKeyHash

toCardanoStakeKeyHash :: PV1.PubKeyHash -> Either ToCardanoError (C.Hash C.StakeKey)
toCardanoStakeKeyHash (PV1.PubKeyHash bs) = tag "toCardanoStakeKeyHash" $ deserialiseFromRawBytes (C.AsHash C.AsStakeKey) (PlutusTx.fromBuiltin bs)

fromCardanoTxOutValue :: C.TxOutValue era -> PV1.Value
fromCardanoTxOutValue (C.TxOutAdaOnly _ lovelace) = fromCardanoLovelace lovelace
fromCardanoTxOutValue (C.TxOutValue _ value)      = fromCardanoValue value

toCardanoTxOutValue :: PV1.Value -> Either ToCardanoError (C.TxOutValue C.BabbageEra)
toCardanoTxOutValue value = do
    when (Ada.fromValue value == mempty) (Left OutputHasZeroAda)
    C.TxOutValue C.MultiAssetInBabbageEra <$> toCardanoValue value

toCardanoTxOutValueUnsafe :: PV1.Value -> Either ToCardanoError (C.TxOutValue C.BabbageEra)
toCardanoTxOutValueUnsafe value = C.TxOutValue C.MultiAssetInBabbageEra <$> toCardanoValue value

fromCardanoTxOutDatumHash :: C.TxOutDatum C.CtxTx era -> Maybe L.DatumHash
fromCardanoTxOutDatumHash C.TxOutDatumNone       = Nothing
fromCardanoTxOutDatumHash (C.TxOutDatumHash _ h) = Just $ PV1.DatumHash $ PlutusTx.toBuiltin (C.serialiseToRawBytes h)
fromCardanoTxOutDatumHash (C.TxOutDatumInTx _ d) = Just $ PV1.DatumHash $ PlutusTx.toBuiltin (C.serialiseToRawBytes (C.hashScriptData d))
fromCardanoTxOutDatumHash (C.TxOutDatumInline _ d) = Just $ PV1.DatumHash $ PlutusTx.toBuiltin (C.serialiseToRawBytes (C.hashScriptData d))

fromCardanoTxOutDatum :: C.TxOutDatum C.CtxTx era -> PV2.OutputDatum
fromCardanoTxOutDatum C.TxOutDatumNone       = PV2.NoOutputDatum
fromCardanoTxOutDatum (C.TxOutDatumHash _ h) = PV2.OutputDatumHash $ PV2.DatumHash $ PlutusTx.toBuiltin (C.serialiseToRawBytes h)
fromCardanoTxOutDatum (C.TxOutDatumInTx _ d) = PV2.OutputDatum $ PV2.Datum $ fromCardanoScriptData d
fromCardanoTxOutDatum (C.TxOutDatumInline _ d) = PV2.OutputDatum $ PV2.Datum $ fromCardanoScriptData d

toCardanoTxOutDatumHash :: Maybe L.DatumHash -> Either ToCardanoError (C.TxOutDatum ctx C.BabbageEra)
toCardanoTxOutDatumHash Nothing   = pure C.TxOutDatumNone
toCardanoTxOutDatumHash (Just dh) = C.TxOutDatumHash C.ScriptDataInBabbageEra <$> toCardanoScriptDataHash dh

toCardanoScriptDataHash :: PV1.DatumHash -> Either ToCardanoError (C.Hash C.ScriptData)
toCardanoScriptDataHash (PV1.DatumHash bs) = tag "toCardanoTxOutDatumHash" (deserialiseFromRawBytes (C.AsHash C.AsScriptData) (PlutusTx.fromBuiltin bs))

fromCardanoMintValue :: C.TxMintValue build era -> PV1.Value
fromCardanoMintValue C.TxMintNone              = mempty
fromCardanoMintValue (C.TxMintValue _ value _) = fromCardanoValue value

<<<<<<< HEAD
toCardanoMintValue :: L.Tx -> Either ToCardanoError (C.TxMintValue C.BuildTx C.BabbageEra)
toCardanoMintValue tx@L.Tx{..} =
    let indexedMps = Map.assocs txMintingScripts
=======
toCardanoMintValue
    :: PV1.Redeemers
    -> PV1.Value
    -> Map.Map P.MintingPolicyHash (P.Versioned P.MintingPolicy)
    -> Either ToCardanoError (C.TxMintValue C.BuildTx C.BabbageEra)
toCardanoMintValue redeemers value mps =
    let indexedMps = Prelude.zip [0..] $ Map.toList mps
>>>>>>> next-node
     in C.TxMintValue C.MultiAssetInBabbageEra
        <$> toCardanoValue txMint
        <*> (C.BuildTxWith . Map.fromList <$> traverse (\(mph, rd) ->
          (,) <$> toCardanoPolicyId mph <*> toCardanoMintWitness rd (L.lookupMintingPolicy (L.txScripts tx) mph)) indexedMps)

fromCardanoValue :: C.Value -> PV1.Value
fromCardanoValue (C.valueToList -> list) = foldMap toValue list
    where
        toValue (C.AdaAssetId, C.Quantity q) = Ada.lovelaceValueOf q
        toValue (C.AssetId policyId assetName, C.Quantity q)
            = Value.singleton (Value.mpsSymbol $ fromCardanoPolicyId policyId) (fromCardanoAssetName assetName) q

toCardanoValue :: PV1.Value -> Either ToCardanoError C.Value
toCardanoValue = fmap C.valueFromList . traverse fromValue . Value.flattenValue
    where
        fromValue (currencySymbol, tokenName, amount)
            | currencySymbol == Ada.adaSymbol && tokenName == Ada.adaToken =
                pure (C.AdaAssetId, C.Quantity amount)
            | otherwise =
                (,) <$> (C.AssetId <$> toCardanoPolicyId (Value.currencyMPSHash currencySymbol) <*> pure (toCardanoAssetName tokenName)) <*> pure (C.Quantity amount)

fromCardanoPolicyId :: C.PolicyId -> PV1.MintingPolicyHash
fromCardanoPolicyId (C.PolicyId scriptHash) = PV1.MintingPolicyHash $ PlutusTx.toBuiltin (C.serialiseToRawBytes scriptHash)

toCardanoPolicyId :: PV1.MintingPolicyHash -> Either ToCardanoError C.PolicyId
toCardanoPolicyId (PV1.MintingPolicyHash bs) = C.PolicyId <$> tag "toCardanoPolicyId" (tag (show (BS.length (PlutusTx.fromBuiltin bs)) <> " bytes") (deserialiseFromRawBytes C.AsScriptHash (PlutusTx.fromBuiltin bs)))

fromCardanoAssetName :: C.AssetName -> Value.TokenName
fromCardanoAssetName (C.AssetName bs) = Value.TokenName $ PlutusTx.toBuiltin bs

toCardanoAssetName :: Value.TokenName -> C.AssetName
toCardanoAssetName (Value.TokenName bs) = C.AssetName $ PlutusTx.fromBuiltin bs

fromCardanoFee :: C.TxFee era -> PV1.Value
fromCardanoFee (C.TxFeeImplicit _)          = mempty
fromCardanoFee (C.TxFeeExplicit _ lovelace) = fromCardanoLovelace lovelace

toCardanoFee :: PV1.Value -> Either ToCardanoError (C.TxFee C.BabbageEra)
toCardanoFee value = C.TxFeeExplicit C.TxFeesExplicitInBabbageEra <$> toCardanoLovelace value

fromCardanoLovelace :: C.Lovelace -> PV1.Value
fromCardanoLovelace (C.lovelaceToQuantity -> C.Quantity lovelace) = Ada.lovelaceValueOf lovelace

toCardanoLovelace :: PV1.Value -> Either ToCardanoError C.Lovelace
toCardanoLovelace value = if value == Ada.lovelaceValueOf lovelace then pure . C.quantityToLovelace . C.Quantity $ lovelace else Left ValueNotPureAda
    where
        Ada.Lovelace lovelace = Ada.fromValue value

fromCardanoValidityRange :: (C.TxValidityLowerBound era, C.TxValidityUpperBound era) -> L.SlotRange
fromCardanoValidityRange (l, u) = PV1.Interval (fromCardanoValidityLowerBound l) (fromCardanoValidityUpperBound u)

toCardanoValidityRange :: L.SlotRange -> Either ToCardanoError (C.TxValidityLowerBound C.BabbageEra, C.TxValidityUpperBound C.BabbageEra)
toCardanoValidityRange (PV1.Interval l u) = (,) <$> toCardanoValidityLowerBound l <*> toCardanoValidityUpperBound u

fromCardanoValidityLowerBound :: C.TxValidityLowerBound era -> PV1.LowerBound L.Slot
fromCardanoValidityLowerBound C.TxValidityNoLowerBound = PV1.LowerBound PV1.NegInf True
fromCardanoValidityLowerBound (C.TxValidityLowerBound _ slotNo) = PV1.LowerBound (PV1.Finite $ fromCardanoSlotNo slotNo) True

toCardanoValidityLowerBound :: PV1.LowerBound L.Slot -> Either ToCardanoError (C.TxValidityLowerBound C.BabbageEra)
toCardanoValidityLowerBound (PV1.LowerBound PV1.NegInf _) = pure C.TxValidityNoLowerBound
toCardanoValidityLowerBound (PV1.LowerBound (PV1.Finite slotNo) closed)
    = pure . C.TxValidityLowerBound C.ValidityLowerBoundInBabbageEra . toCardanoSlotNo $ if slotNo < 0 then 0 else if closed then slotNo else slotNo + 1
toCardanoValidityLowerBound (PV1.LowerBound PV1.PosInf _) = Left InvalidValidityRange

fromCardanoValidityUpperBound :: C.TxValidityUpperBound era -> PV1.UpperBound L.Slot
fromCardanoValidityUpperBound (C.TxValidityNoUpperBound _) = PV1.UpperBound PV1.PosInf True
fromCardanoValidityUpperBound (C.TxValidityUpperBound _ slotNo) = PV1.UpperBound (PV1.Finite $ fromCardanoSlotNo slotNo) False

toCardanoValidityUpperBound :: PV1.UpperBound L.Slot -> Either ToCardanoError (C.TxValidityUpperBound C.BabbageEra)
toCardanoValidityUpperBound (PV1.UpperBound PV1.PosInf _) = pure $ C.TxValidityNoUpperBound C.ValidityNoUpperBoundInBabbageEra
toCardanoValidityUpperBound (PV1.UpperBound (PV1.Finite slotNo) closed)
    = pure . C.TxValidityUpperBound C.ValidityUpperBoundInBabbageEra . toCardanoSlotNo $ if closed then slotNo + 1 else slotNo
toCardanoValidityUpperBound (PV1.UpperBound PV1.NegInf _) = Left InvalidValidityRange

fromCardanoSlotNo :: C.SlotNo -> L.Slot
fromCardanoSlotNo (C.SlotNo w64) = L.Slot (toInteger w64)

toCardanoSlotNo :: L.Slot -> C.SlotNo
toCardanoSlotNo (L.Slot i) = C.SlotNo (fromInteger i)

fromCardanoScriptData :: C.ScriptData -> PV1.BuiltinData
fromCardanoScriptData = PV1.dataToBuiltinData . C.toPlutusData

toCardanoScriptData :: PV1.BuiltinData -> C.ScriptData
toCardanoScriptData = C.fromPlutusData . PV1.builtinDataToData

<<<<<<< HEAD
fromCardanoScriptInEra :: C.ScriptInEra era -> Maybe (L.Script, L.LedgerPlutusVersion)
fromCardanoScriptInEra (C.ScriptInEra C.PlutusScriptV1InAlonzo (C.PlutusScript C.PlutusScriptV1 script)) =
    Just (fromCardanoPlutusScript script, L.PlutusV1)
fromCardanoScriptInEra (C.ScriptInEra C.PlutusScriptV1InBabbage (C.PlutusScript C.PlutusScriptV1 script)) =
    Just (fromCardanoPlutusScript script, L.PlutusV1)
fromCardanoScriptInEra (C.ScriptInEra C.PlutusScriptV2InBabbage (C.PlutusScript C.PlutusScriptV2 script)) =
    Just (fromCardanoPlutusScript script, L.PlutusV2)
fromCardanoScriptInEra (C.ScriptInEra _ C.SimpleScript{}) = Nothing

toCardanoScriptInEra :: L.Script -> L.LedgerPlutusVersion -> Either ToCardanoError (C.ScriptInEra C.BabbageEra)
toCardanoScriptInEra script L.PlutusV1 = C.ScriptInEra C.PlutusScriptV1InBabbage . C.PlutusScript C.PlutusScriptV1 <$> toCardanoPlutusScript (C.AsPlutusScript C.AsPlutusScriptV1) script
toCardanoScriptInEra script L.PlutusV2 = C.ScriptInEra C.PlutusScriptV2InBabbage . C.PlutusScript C.PlutusScriptV2 <$> toCardanoPlutusScript (C.AsPlutusScript C.AsPlutusScriptV2) script
=======
fromCardanoScriptInEra :: C.ScriptInEra era -> Maybe (P.Versioned P.Script)
fromCardanoScriptInEra (C.ScriptInEra C.PlutusScriptV1InAlonzo (C.PlutusScript C.PlutusScriptV1 script)) =
    Just (P.Versioned (fromCardanoPlutusScript script) P.PlutusV1)
fromCardanoScriptInEra (C.ScriptInEra C.PlutusScriptV1InBabbage (C.PlutusScript C.PlutusScriptV1 script)) =
    Just (P.Versioned (fromCardanoPlutusScript script) P.PlutusV1)
fromCardanoScriptInEra (C.ScriptInEra C.PlutusScriptV2InBabbage (C.PlutusScript C.PlutusScriptV2 script)) =
    Just (P.Versioned (fromCardanoPlutusScript script) P.PlutusV2)
fromCardanoScriptInEra (C.ScriptInEra _ C.SimpleScript{}) = Nothing

toCardanoScriptInEra :: P.Versioned P.Script -> Either ToCardanoError (C.ScriptInEra C.BabbageEra)
toCardanoScriptInEra (P.Versioned script P.PlutusV1) = C.ScriptInEra C.PlutusScriptV1InBabbage . C.PlutusScript C.PlutusScriptV1 <$> toCardanoPlutusScript (C.AsPlutusScript C.AsPlutusScriptV1) script
toCardanoScriptInEra (P.Versioned script P.PlutusV2) = C.ScriptInEra C.PlutusScriptV2InBabbage . C.PlutusScript C.PlutusScriptV2 <$> toCardanoPlutusScript (C.AsPlutusScript C.AsPlutusScriptV2) script
>>>>>>> next-node

fromCardanoPlutusScript :: C.HasTypeProxy lang => C.PlutusScript lang -> PV1.Script
fromCardanoPlutusScript = Codec.deserialise . BSL.fromStrict . C.serialiseToRawBytes

toCardanoPlutusScript
    :: C.SerialiseAsRawBytes plutusScript
    => C.AsType plutusScript
    -> L.Script
    -> Either ToCardanoError plutusScript
toCardanoPlutusScript asPlutusScriptType =
    tag "toCardanoPlutusScript"
    . deserialiseFromRawBytes asPlutusScriptType . BSL.toStrict . Codec.serialise

<<<<<<< HEAD
fromCardanoScriptInAnyLang :: C.ScriptInAnyLang -> Maybe (L.Script, L.LedgerPlutusVersion)
fromCardanoScriptInAnyLang (C.ScriptInAnyLang _sl (C.SimpleScript _ssv _ss)) = Nothing
fromCardanoScriptInAnyLang (C.ScriptInAnyLang _sl (C.PlutusScript psv ps)) = Just $ case psv of
     C.PlutusScriptV1 -> (fromCardanoPlutusScript ps, L.PlutusV1)
     C.PlutusScriptV2 -> (fromCardanoPlutusScript ps, L.PlutusV2)
=======
fromCardanoScriptInAnyLang :: C.ScriptInAnyLang -> Maybe (P.Versioned P.Script)
fromCardanoScriptInAnyLang (C.ScriptInAnyLang _sl (C.SimpleScript _ssv _ss)) = Nothing
fromCardanoScriptInAnyLang (C.ScriptInAnyLang _sl (C.PlutusScript psv ps)) = Just $ case psv of
     C.PlutusScriptV1 -> P.Versioned (fromCardanoPlutusScript ps) P.PlutusV1
     C.PlutusScriptV2 -> P.Versioned (fromCardanoPlutusScript ps) P.PlutusV2

toCardanoScriptInAnyLang :: P.Versioned P.Script -> Either ToCardanoError C.ScriptInAnyLang
toCardanoScriptInAnyLang (P.Versioned script P.PlutusV1) =
  C.ScriptInAnyLang (C.PlutusScriptLanguage C.PlutusScriptV1) . C.PlutusScript C.PlutusScriptV1
    <$> toCardanoPlutusScript (C.AsPlutusScript C.AsPlutusScriptV1) script
toCardanoScriptInAnyLang (P.Versioned script P.PlutusV2) =
  C.ScriptInAnyLang (C.PlutusScriptLanguage C.PlutusScriptV2) . C.PlutusScript C.PlutusScriptV2
    <$> toCardanoPlutusScript (C.AsPlutusScript C.AsPlutusScriptV2) script
>>>>>>> next-node

deserialiseFromRawBytes :: C.SerialiseAsRawBytes t => C.AsType t -> ByteString -> Either ToCardanoError t
deserialiseFromRawBytes asType = maybe (Left DeserialisationError) Right . C.deserialiseFromRawBytes asType

tag :: String -> Either ToCardanoError t -> Either ToCardanoError t
tag s = first (Tag s)

data FromCardanoError
    = SimpleScriptsNotSupported
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON, ToObject)

instance Pretty FromCardanoError where
    pretty SimpleScriptsNotSupported        = "Simple scripts are not supported"

data ToCardanoError
    = TxBodyError String -- ^ A C.TxBodyError converted to String
    | DeserialisationError
    | InvalidValidityRange
    | ValueNotPureAda
    | OutputHasZeroAda
    | StakingPointersNotSupported
    | SimpleScriptsNotSupportedToCardano
    | MissingInputValidator
    | MissingDatum
    | MissingMintingPolicy
    | ScriptPurposeNotSupported PV1.ScriptTag
<<<<<<< HEAD
    | UnsupportedPlutusVersion L.LedgerPlutusVersion
    | MissingStakeValidator
=======
    | UnsupportedPlutusVersion P.Language
>>>>>>> next-node
    | Tag String ToCardanoError
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

instance Pretty ToCardanoError where
    pretty (TxBodyError err)                  = "TxBodyError" <> colon <+> pretty err
    pretty DeserialisationError               = "ByteString deserialisation failed"
    pretty InvalidValidityRange               = "Invalid validity range"
    pretty ValueNotPureAda                    = "Fee values should only contain Ada"
    pretty OutputHasZeroAda                   = "Transaction outputs should not contain zero Ada"
    pretty StakingPointersNotSupported        = "Staking pointers are not supported"
    pretty SimpleScriptsNotSupportedToCardano = "Simple scripts are not supported"
    pretty MissingMintingPolicy               = "Missing minting policy"
    pretty (ScriptPurposeNotSupported p)      = "Script purpose not supported:" <+> viaShow p
    pretty (UnsupportedPlutusVersion v)       = "Plutus version not supported:" <+> viaShow v
    pretty MissingInputValidator              = "Missing input validator."
    pretty MissingDatum                       = "Missing required datum."
    pretty MissingStakeValidator              = "Missing stake validator."
    pretty (Tag t err)                        = pretty t <> colon <+> pretty err

zeroExecutionUnits :: C.ExecutionUnits
zeroExecutionUnits = C.ExecutionUnits 0 0

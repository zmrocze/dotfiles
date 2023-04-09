{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

{-| The chain index' version of a transaction
-}
module Plutus.ChainIndex.Tx(
    ChainIndexTx(..)
    , ChainIndexTxOutputs(..)
    , fromOnChainTx
    , txOutRefs
    , txOutsWithRef
    , txOutRefMap
    , txOutRefMapForAddr
    , txRedeemersWithHash
    -- ** Lenses
    , citxTxId
    , citxInputs
    , citxOutputs
    , citxValidRange
    , citxData
    , citxRedeemers
    , citxScripts
    , citxCardanoTx
    , _InvalidTx
    , _ValidTx
    ) where

import Control.Arrow ((&&&))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set
import Data.Tuple (swap)
import Ledger (Address, OnChainTx (..), SomeCardanoApiTx (SomeTx), Tx (..), TxIn (..), TxInType (..),
               TxOut (txOutAddress), TxOutRef (..), onCardanoTx, txId)
import Ledger (Address, OnChainTx (..), SomeCardanoApiTx (SomeTx), Tx (..), TxOut (txOutAddress), TxOutRef (..),
               onCardanoTx, txId)
import Ledger.Scripts (Redeemer, RedeemerHash)
import Ledger.Tx (Certificate (certificateRedeemer), TxInput (txInputType), TxInputType (TxConsumeScriptAddress),
                  Withdrawal (withdrawalRedeemer), fillTxInputWitnesses)
import Plutus.Contract.CardanoAPI (fromCardanoTx, setValidity)
import Plutus.Script.Utils.V1.Scripts (redeemerHash)

import Plutus.ChainIndex.Types
import Plutus.Contract.CardanoAPI (fromCardanoTx, setValidity)
import Plutus.Script.Utils.Scripts (datumHash, redeemerHash)
import Plutus.Script.Utils.V1.Scripts (mintingPolicyHash, validatorHash)
import Plutus.V1.Ledger.Scripts (Datum, DatumHash, MintingPolicy (getMintingPolicy),
                                 MintingPolicyHash (MintingPolicyHash), Redeemer, RedeemerHash, Script, ScriptHash (..),
                                 Validator (getValidator), ValidatorHash (ValidatorHash))
import Plutus.V1.Ledger.Tx (RedeemerPtr (RedeemerPtr), Redeemers, ScriptTag (Spend))

-- | Get tx output references from tx.
txOutRefs :: ChainIndexTx -> [TxOutRef]
txOutRefs ChainIndexTx { _citxTxId, _citxOutputs = ValidTx outputs } =
    map (\idx -> TxOutRef _citxTxId idx) [0 .. fromIntegral $ length outputs - 1]
txOutRefs ChainIndexTx{_citxOutputs = InvalidTx} = []

-- | Get tx output references and tx outputs from tx.
txOutsWithRef :: ChainIndexTx -> [(TxOut, TxOutRef)]
txOutsWithRef tx@ChainIndexTx { _citxOutputs = ValidTx outputs } = zip outputs $ txOutRefs tx
txOutsWithRef ChainIndexTx { _citxOutputs = InvalidTx }          = []

-- | Get 'Map' of tx outputs references to tx.
txOutRefMap :: ChainIndexTx -> Map TxOutRef (TxOut, ChainIndexTx)
txOutRefMap tx =
    fmap (, tx) $ Map.fromList $ fmap swap $ txOutsWithRef tx

-- | Get 'Map' of tx outputs from tx for a specific address.
txOutRefMapForAddr :: Address -> ChainIndexTx -> Map TxOutRef (TxOut, ChainIndexTx)
txOutRefMapForAddr addr tx =
    Map.filter ((==) addr . txOutAddress . fst) $ txOutRefMap tx

-- | Convert a 'OnChainTx' to a 'ChainIndexTx'. An invalid 'OnChainTx' will not
-- produce any 'ChainIndexTx' outputs and the collateral inputs of the
-- 'OnChainTx' will be the inputs of the 'ChainIndexTx'.
fromOnChainTx :: OnChainTx -> ChainIndexTx
fromOnChainTx = \case
    Valid ctx -> onCardanoTx
        (\case tx@Tx{txInputs, txOutputs, txValidRange, txData, txScripts, txWithdrawals, txCertificates, txMintingScripts} ->
                let redeemers = allRedeemers txWithdrawals txCertificates txMintingScripts txInputs in
                ChainIndexTx
                    { _citxTxId = txId tx
<<<<<<< HEAD
                    , _citxInputs = Set.toList txInputs
=======
                    , _citxInputs = Set.fromList $ map (fillTxInputWitnesses tx) txInputs
>>>>>>> update-tx-on-main
                    , _citxOutputs = ValidTx txOutputs
                    , _citxValidRange = txValidRange
                    , _citxData = txData
                    , _citxRedeemers = redeemersToMap redeemers
                    , _citxScripts = txScripts
                    , _citxCardanoTx = Nothing
                    }
        )
        (fromOnChainCardanoTx True)
        ctx
    Invalid ctx -> onCardanoTx
        (\case tx@Tx{txCollateral, txValidRange, txData, txScripts, txMintingScripts, txWithdrawals, txCertificates} ->
                let redeemers = allRedeemers txWithdrawals txCertificates txMintingScripts txCollateral in
                ChainIndexTx
                    { _citxTxId = txId tx
<<<<<<< HEAD
                    , _citxInputs = Set.toList txCollateral
=======
                    , _citxInputs = Set.fromList $ map (fillTxInputWitnesses tx) txCollateral
>>>>>>> update-tx-on-main
                    , _citxOutputs = InvalidTx
                    , _citxValidRange = txValidRange
                    , _citxData = txData
                    , _citxRedeemers = redeemersToMap redeemers
                    , _citxScripts = txScripts
                    , _citxCardanoTx = Nothing
                    })
        (fromOnChainCardanoTx False)
        ctx
    where
        redeemersToMap :: [Redeemer] -> Map RedeemerHash Redeemer
        redeemersToMap = Map.fromList . map (redeemerHash &&& id)

        allRedeemers txWithdrawals txCertificates txMintingScripts txInputs =
            mapMaybe withdrawalRedeemer txWithdrawals
            <> mapMaybe certificateRedeemer txCertificates
            <> Map.elems txMintingScripts
            <> mapMaybe (
                (\case
                    TxConsumeScriptAddress rd _ _ -> Just rd
                    _                             -> Nothing) . txInputType) txInputs

-- Cardano api transactions store validity internally. Our emulated blockchain stores validity outside of the transactions,
-- so we need to make sure these match up. Once we only have cardano api txs this can be removed.
fromOnChainCardanoTx :: Bool -> SomeCardanoApiTx -> ChainIndexTx
fromOnChainCardanoTx validity (SomeTx tx era) =
    either (error . ("Plutus.ChainIndex.Tx.fromOnChainCardanoTx: " ++) . show) id $ fromCardanoTx era $ setValidity validity tx
<<<<<<< HEAD

mintingPolicies :: Set MintingPolicy -> Map ScriptHash Script
mintingPolicies = Map.fromList . fmap withHash . Set.toList
  where
    withHash mp = let (MintingPolicyHash mph) = mintingPolicyHash mp
                   in (ScriptHash mph, getMintingPolicy mp)

validators :: Set TxIn -> (Map ScriptHash Script, Map DatumHash Datum, Redeemers)
validators txIns = foldMap (\(ix, txIn) -> maybe mempty (withHash ix) $ txInType txIn) $ zip [0..] (Set.toList txIns)
  where
    -- TODO: the index of the txin is probably incorrect as we take it from the set.
    -- To determine the proper index we have to convert the plutus's `TxIn` to cardano-api `TxIn` and
    -- sort them by using the standard `Ord` instance.
    withHash ix (ConsumeScriptAddress val red dat) =
      let (ValidatorHash vh) = validatorHash val
       in ( Map.singleton (ScriptHash vh) (getValidator val)
          , Map.singleton (datumHash dat) dat
          , Map.singleton (RedeemerPtr Spend ix) red
          )
    withHash _ _ = mempty

txRedeemersWithHash :: ChainIndexTx -> Map RedeemerHash Redeemer
txRedeemersWithHash ChainIndexTx{_citxRedeemers} = Map.fromList
    $ fmap (\r -> (redeemerHash r, r))
    $ Map.elems _citxRedeemers
=======
>>>>>>> update-tx-on-main

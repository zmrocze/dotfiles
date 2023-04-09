{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost        #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NumericUnderscores         #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE QuantifiedConstraints      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}
{-# OPTIONS_GHC -Wno-redundant-constraints -fno-warn-name-shadowing #-}

module Plutus.Contract.Test.ContractModel.DoubleSatisfaction
    ( checkDoubleSatisfaction
    , checkDoubleSatisfactionWithOptions
    ) where

import PlutusTx qualified
import PlutusTx.Builtins hiding (error)



import Control.Lens
import Control.Monad.Cont
import Control.Monad.Freer (Eff, run)
import Control.Monad.Freer.Extras.Log (LogMessage, logMessageContent)
import Control.Monad.State qualified as State
import Data.Default
import Data.Either
import Data.Map qualified as Map
import Data.Maybe
import Data.Set qualified as Set
import Ledger.Params (Params)

import Ledger (unPaymentPrivateKey, unPaymentPubKeyHash)
import Ledger.Crypto
import Ledger.Generators
import Ledger.Index as Index
import Ledger.Scripts
import Ledger.Slot
import Ledger.Tx hiding (mint)
import Ledger.Validation qualified as Validation
import Ledger.Value (adaOnlyValue)
import Plutus.Contract.Test hiding (not)
import Plutus.Contract.Test.ContractModel.Internal
import Plutus.Script.Utils.V1.Scripts (datumHash, validatorHash)
import Plutus.Trace.Emulator as Trace (EmulatorTrace, activateContract, callEndpoint, runEmulatorStream)
import Plutus.V1.Ledger.Address
import Plutus.V1.Ledger.TxId
import Streaming qualified as S
import Test.QuickCheck.StateModel hiding (Action, Actions (..), actionName, arbitraryAction, initialState, monitoring,
                                   nextState, pattern Actions, perform, precondition, shrinkAction, stateAfter)
import Test.QuickCheck.StateModel qualified as StateModel

import Test.QuickCheck hiding (ShrinkState, checkCoverage, getSize, (.&&.), (.||.))
import Test.QuickCheck.Monadic (monadic)
import Test.QuickCheck.Monadic qualified as QC

import Wallet.Emulator.Chain hiding (_currentSlot, currentSlot)
import Wallet.Emulator.MultiAgent (EmulatorEvent, EmulatorEvent' (ChainEvent), eteEvent)
import Wallet.Emulator.Stream (EmulatorErr)


import Prettyprinter
import Plutus.V1.Ledger.Tx (outAddress, isPubKeyOut)

-- Double satisfaction magic

-- | A transaction packed up with enough of the chain state to be able to run validation. This is
--   used both for legitimate transactions and for the transactions manufactured by the double
--   satisfaction test.
data WrappedTx = WrappedTx
  { _dsTxId      :: TxId
  , _dsTx        :: Tx
  , _dsUtxoIndex :: UtxoIndex
  , _dsSlot      :: Slot
  } deriving Show
makeLenses ''WrappedTx

-- | Perform a light-weight check to find egregious double satisfaction
-- vulnerabilities in contracts.
--
-- A counterexample to this property consists of three transactions.
-- * The first transaction is a valid transaction from the trace generated by the contract model.
-- * The second transaction, generated by redirecting
--   a non-datum pubkey output from a non-signer to a signer in the first transaction,
--   fails to validate. This demonstrates that funds can't simply be stolen.
-- * The third transaction goes through and manages to steal funds by altering the first transaction.
--   It is generated by adding another script input (with the same value as the non-signer non-stealable
--   pubkey output) and adding a datum to the non-signer non-stealable pubkey output, and giving the
--   extra value from the new script input to a signer.
checkDoubleSatisfaction :: forall m. ContractModel m
                        => Actions m
                        -> Property
checkDoubleSatisfaction = checkDoubleSatisfactionWithOptions defaultCheckOptionsContractModel
                                                             defaultCoverageOptions

-- | Perform a light-weight check to find egregious double satisfaction
-- vulnerabilities in contracts, with options.
checkDoubleSatisfactionWithOptions :: forall m. ContractModel m
                                   => CheckOptions
                                   -> CoverageOptions
                                   -> Actions m
                                   -> Property
checkDoubleSatisfactionWithOptions opts covopts acts =
  property . monadic (flip State.evalState mempty)
           $ finalChecks opts covopts (finalPredicate finalState (const (pure True))) $ do
   QC.run initiateWallets
   env <- snd <$> runActionsInState StateModel.initialState (toStateModelActions acts)
   ContractMonadState tr _ _ <- QC.run State.get
   let innerAction :: EmulatorTrace AssetMap
       innerAction = State.execStateT (runEmulatorAction tr IMNil) Map.empty

       action = do
         -- see note [The Env contract]
         env <- innerAction
         hdl <- activateContract w1 (getEnvContract @()) envContractInstanceTag
         void $ callEndpoint @"register-token-env" hdl env

       stream :: forall effs. S.Stream (S.Of (LogMessage EmulatorEvent)) (Eff effs) (Maybe EmulatorErr)
       stream = fst <$> runEmulatorStream (opts ^. emulatorConfig) action

       (errorResult, events) = S.streamFold (,[]) run (\ (msg S.:> es) -> (fst es, (msg ^. logMessageContent) : snd es)) stream

       chainEvents :: [ChainEvent]
       chainEvents = [ ce | ChainEvent ce <- view eteEvent <$> events ]

   case errorResult of
     Just err -> do
       QC.monitor $ counterexample (show err)
       QC.assert False
     _ -> return ()

   QC.monitor $ tabulate "Number of ChainEvents" (bucket 10 $ length chainEvents)
   QC.monitor $ tabulate "ChainEvent type" (map chainEventType chainEvents)
   case getDSCounterexamples chainEvents of
    (cands, potentialCEs, []) -> do
      QC.monitor $ tabulate "Validating candidate counterexamples?" [ show $ validateWrappedTx c
                                                                    | c <- cands ]
      QC.monitor $ tabulate "Number of candidates to build counterexamples" (bucket 10 $ length cands)
      QC.monitor $ tabulate "Number of candidate counterexamples" (bucket 10 $ length potentialCEs)
      QC.monitor $ tabulate "Validate counterexample result"
        [ show (validateWrappedTx c0) ++ ", " ++
          show (validateWrappedTx c1)
        | DoubleSatisfactionCounterexample _ c0 c1 _ _ _ <- potentialCEs ]
      QC.monitor $ tabulate "Reasons the steal candidate (that shouldn't work) is rejected"
        [ head . words . show $ r
        | DoubleSatisfactionCounterexample _ c0 c1 _ _ _ <- potentialCEs
        , Prelude.not $ validateWrappedTx c0
        , Just r <- [validateWrappedTx' c1] ]
      QC.monitor $ tabulate "Reasons it doesn't work"
        [ head . words . show $ r
        | DoubleSatisfactionCounterexample _ c0 c1 _ _ _ <- potentialCEs
        , Prelude.not $ validateWrappedTx c0
        , Just r <- [validateWrappedTx' c1] ]
    (_, _, counterexamples)  -> do
      sequence_ [ do QC.monitor $ counterexample (showPretty c)
                | c <- counterexamples ]
      QC.assert False
   return env
    where
      chainEventType (TxnValidate _ constr ces) = "TxnValidate "
        ++ (head . words . show $ constr)
        ++ " " ++ concat [ if isLeft (sveResult ce) then "E" else "_" | ce <- ces ]
      chainEventType ce = head . words . show $ ce

      finalState = StateModel.stateAfter (toStateModelActions acts)

-- | Given a list of chain events, computes a triple of
--    * the list of transactions that were considered for double satisfaction vulnerabilities
--    * the list of candidate counterexamples, i.e. transactions where there were funds that could
--      potentially be stolen
--    * the list of actual counterexamples, i.e. if this is non-empty a vulnerability has been
--      discovered.
getDSCounterexamples :: [ChainEvent] -> ( [WrappedTx]
                                        , [DoubleSatisfactionCounterexample]
                                        , [DoubleSatisfactionCounterexample]
                                        )
getDSCounterexamples cs = go 0 mempty cs
  where
    go _ _ [] = ([], [], [])
    go slot idx (e:es) = case e of
      SlotAdd slot' -> go slot' idx es
      TxnValidate _ txn@(view -> Just tx) ces
        | all (isRight . sveResult) ces ->
          let idx' = case fst $ runValidation (validateTransaction slot tx)
                                              (ValidationCtx idx def) of
                       Just (Index.Phase1, _) -> idx
                       Just (Index.Phase2, _) -> Index.insertCollateral txn idx
                       Nothing                -> Index.insert txn idx
              cands = doubleSatisfactionCandidates slot idx e
              potentialCEs = doubleSatisfactionCounterexamples =<< cands
              actualCEs = checkForDoubleSatisfactionVulnerability slot idx e
              (candsRest, potentialRest, counterexamplesRest) = go slot idx' es
          in (cands ++ candsRest, potentialCEs ++ potentialRest, actualCEs ++ counterexamplesRest)
        | otherwise                    -> go slot idx es
      -- NOTE: We are not including spent collateral inputs here, but that's fine because
      -- the transactions we mutate are never mutated to include these unspent inputs. We only
      -- need to keep track of what UTxOs exist for the validator to validate the transactions
      -- that are created by adding UTxOs from thin air to existing, already validating,
      -- transactions. In the future if you want to do something more interesting to the mutators
      -- you may need to be more careful here (or, you know, rewrite all this code from scratch).
      _ -> go slot idx es

    view (Both tx _)     = Just tx
    view (EmulatorTx tx) = Just tx
    view _               = Nothing

-- | Take a chain event and wrap it up as a `WrappedTx` if it was a transaction
--   validation event.
doubleSatisfactionCandidates :: Slot -> UtxoIndex -> ChainEvent -> [WrappedTx]
doubleSatisfactionCandidates slot idx event = case event of
  TxnValidate txid (EmulatorTx tx) _ -> [WrappedTx txid tx idx slot]
  TxnValidate txid (Both tx _) _     -> [WrappedTx txid tx idx slot]
  _                                  -> []

-- | Run validation for a `WrappedTx`. Returns @Nothing@ if successful and @Just err@ if validation
--   failed with error @err@.
validateWrappedTx' :: WrappedTx -> Maybe ValidationErrorInPhase
validateWrappedTx' cand = fst $ runValidation (validateTransaction (cand ^. dsSlot) (cand ^. dsTx))
                                              (ValidationCtx (cand ^.dsUtxoIndex) def)

-- | Run validation for a `WrappedTx`. Returns @True@ if successful.
validateWrappedTx :: WrappedTx -> Bool
validateWrappedTx = isNothing . validateWrappedTx'

-- | Actual counterexamples showing a double satisfaction vulnerability for the given chain event.
checkForDoubleSatisfactionVulnerability :: Slot -> UtxoIndex -> ChainEvent -> [DoubleSatisfactionCounterexample]
checkForDoubleSatisfactionVulnerability slot idx = filter isVulnerable
                                                 . doubleSatisfactionCounterexamples
                                                 <=< doubleSatisfactionCandidates slot idx

-- | This is an actual counterexample if the first transaction passes validation, the second fails,
--   and the third passes.
data DoubleSatisfactionCounterexample = DoubleSatisfactionCounterexample
  { dsceOriginalTransaction :: WrappedTx
      -- ^ The original transaction goes through.
  , dsceTargetMattersProof  :: WrappedTx
      -- ^ If this fails to validate, it's worth checking for a double satisfaction vulnerability.
      --   Generated by redirecting a non-datum pubkey output from a non-signer to a signer.
  , dsceValueStolenProof    :: WrappedTx
      -- ^ If this candidate validates there is a double satisfaction vulnerability. Generated by adding
      --   another script input (with the same value as the non-signer non-stealable pubkey output)
      --   and adding a datum to the non-signer non-stealable pubkey output, and giving the
      --   extra value from the new script input to a signer.
      --
      --   The scenario is that the other script is using a unique datum to identify the payment to
      --   the non-signer pubkey as coming from that other script.
  , dsceStolenUTxO          :: TxOut
  , dsceStealerWallet       :: Wallet
  , dsceDatumUTxO           :: TxOut
  } deriving Show

showPretty :: DoubleSatisfactionCounterexample -> String
showPretty cand = show . vcat $
  [ "=====Double Satisfaction Counterexample!====="
  , "The following transaction goes through:"
  , ""
  , pretty $ cand ^. to dsceOriginalTransaction . dsTx
  , ""
  , "Whereas the following transaction fails:"
  , ""
  , pretty $ cand ^. to dsceTargetMattersProof . dsTx
  , ""
  , "Showing that we can't simply re-direct UTxO"
  , ""
  , pretty $ dsceStolenUTxO cand
  , ""
  , "to wallet " <> pretty (dsceStealerWallet cand) <> ". However, the following transaction goes through:"
  , ""
  , pretty $ cand ^. to dsceValueStolenProof . dsTx
  , ""
  , "which demonstrates that when another script uses a datum on the following UTxO"
  , ""
  , pretty $ dsceDatumUTxO cand
  , ""
  , "(that isn't visible in the pretty printer), to uniquely identify the payment, "
    <> "we can redirect the UTxO to wallet " <> pretty (dsceStealerWallet cand) <> "."
  , ""
  , "For reference the UTxOs indices above correspond to:"
  ] ++
  [ vcat [ pretty (ref ^. inRef)
         , pretty . fromJust $ Map.lookup (ref ^. inRef)
                         (cand ^. to dsceValueStolenProof . dsUtxoIndex . to getIndex)
         ]
  | let tx0 = cand ^. to dsceTargetMattersProof . dsTx
        tx1 = cand ^. to dsceValueStolenProof . dsTx
        tx2 = cand ^. to dsceOriginalTransaction . dsTx
  , ref <- Set.toList $  tx0 ^. inputs
                      <> tx1 ^. inputs
                      <> tx2 ^. inputs
                      <> tx0 ^. collateralInputs
                      <> tx1 ^. collateralInputs
                      <> tx2 ^. collateralInputs
  ]

isVulnerable :: DoubleSatisfactionCounterexample -> Bool
isVulnerable (DoubleSatisfactionCounterexample orig pre post _ _ _) =
  validateWrappedTx orig &&
  Prelude.not (validateWrappedTx pre) &&
  validateWrappedTx post

-- TODO: change this to be the actual validator that only accepts wallet payments with
-- a specific datum attached. Even though this doesn't technically matter.
--
-- This is not super important, but we want to leave no room for misunderstanding...
alwaysOkValidator :: Validator
alwaysOkValidator = mkValidatorScript $$(PlutusTx.compile [|| (\_ _ _ -> ()) ||])

doubleSatisfactionCounterexamples :: WrappedTx -> [DoubleSatisfactionCounterexample]
doubleSatisfactionCounterexamples dsc =
  [ DoubleSatisfactionCounterexample
      { dsceOriginalTransaction = dsc
      , dsceTargetMattersProof  = targetMatters1
      , dsceValueStolenProof    = valueStolen1
      , dsceStolenUTxO          = out
      , dsceDatumUTxO           = withDatumOut
      , dsceStealerWallet       = stealerWallet
      }
  -- For each output in the candidate tx
  | (idx, out) <- zip [0..] (dsc ^. dsTx . outputs)
  , let l = dsTx . outputs. ix idx
  -- Is it a pubkeyout?
  , isPubKeyOut out
  -- Whose key is not in the signatories?
  , key <- maybeToList . txOutPubKey $ out
  , let signatories = dsc ^. dsTx . signatures . to Map.keys
  , key `notElem` map pubKeyHash signatories
  -- For now we only consider one stealer at the time
  , length signatories == 1
  -- Then stealerKey can try to steal it
  , stealerKey <- signatories
  , (stealerWallet, stealerPrivKey) <-
      filter (\(w, _) -> unPaymentPubKeyHash (mockWalletPaymentPubKeyHash w) == pubKeyHash stealerKey)
             (zip knownWallets (unPaymentPrivateKey <$> knownPaymentPrivateKeys))
  , let stealerAddr = pubKeyHashAddress . pubKeyHash $ stealerKey
  -- The output going to the original recipient but with a datum
  , let datum         = Datum . mkB $ "<this is a unique string>"
        datumEmpty    = Datum . mkB $ ""
        redeemerEmpty = Redeemer . mkB $ ""
        withDatumOut = out { txOutDatumHash = Just $ datumHash datum }
        newFakeTxScriptOut = TxOut { txOutAddress   = scriptHashAddress $ validatorHash alwaysOkValidator
                                   , txOutValue     = adaOnlyValue $ txOutValue out
                                   , txOutDatumHash = Just $ datumHash datumEmpty
                                   }
        newFakeTxOutRef = TxOutRef { txOutRefId  = TxId "very sha 256 hash I promise"
                                   , txOutRefIdx = 1
                                   }
        newFakeTxIn = TxIn { txInRef = newFakeTxOutRef
                           , txInType = Just $ ConsumeScriptAddress alwaysOkValidator
                                                                    redeemerEmpty
                                                                    datumEmpty
                           }
  , let targetMatters0 = dsc & l . outAddress .~ stealerAddr
        tx             = addSignature' stealerPrivKey (targetMatters0 ^. dsTx & signatures .~ mempty)
        targetMatters1 = targetMatters0 & dsTxId .~ txId tx
                                        & dsTx   .~ tx
  , let valueStolen0 = dsc & l . outAddress .~ stealerAddr
                           & dsTx . outputs %~ (withDatumOut:)
                           & dsTx . inputs %~ (Set.insert newFakeTxIn)
                           & dsUtxoIndex %~
                              (\ (UtxoIndex m) -> UtxoIndex $ Map.insert newFakeTxOutRef
                                                                         newFakeTxScriptOut m)
                           & dsTx . datumWitnesses . at (datumHash datum) .~ Just datum
                           & dsTx . datumWitnesses . at (datumHash datumEmpty) .~ Just datumEmpty
        tx           = addSignature' stealerPrivKey (valueStolen0 ^. dsTx & signatures .~ mempty)
        valueStolen1 = valueStolen0 & dsTxId .~ txId tx
                                    & dsTx   .~ tx
  ]

toCardanoUtxoIndex :: Params -> UtxoIndex -> Validation.UTxO EmulatorEra
toCardanoUtxoIndex params idx = either (error . show) id $ Validation.fromPlutusIndex params idx


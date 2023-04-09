{-# LANGUAGE OverloadedStrings #-}

module Spec.Integration (test) where

import BotPlutusInterface.Types (LogContext (ContractLog), LogLevel (Error), LogType (AnyLog))
import Control.Exception (ErrorCall, Exception (fromException))
import Control.Monad (void)
import Data.Default (Default (def))
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Map qualified as Map
import Data.Maybe (isJust)
import Data.Text (Text, isInfixOf, pack)
import Ledger (Address (Address), PaymentPubKeyHash (PaymentPubKeyHash), StakePubKeyHash (StakePubKeyHash))
import Ledger.Ada (lovelaceValueOf)
import Ledger.Constraints (MkTxError (OwnPubKeyMissing))
import Plutus.Contract (
  ContractError (ConstraintResolutionContractError),
  throwError,
  waitNSlots,
 )
import Plutus.Contract qualified as Contract
import Plutus.V2.Ledger.Api (Credential (PubKeyCredential), StakingCredential (StakingHash))
import Spec.TestContract.AdjustTx (runAdjustTest)
import Spec.TestContract.AlwaysFail (lockThenFailToSpend)
import Spec.TestContract.LockSpendMint (lockThenSpend)
import Spec.TestContract.SimpleContracts (
  getUtxos,
  getUtxosThrowsErr,
  getUtxosThrowsEx,
  ownValue,
  ownValueToState,
  payTo,
  payToPubKeyAddress,
 )
import Spec.TestContract.ValidateTimeRange (failingTimeContract, successTimeContract)
import Test.Plutip.Contract (
  ClusterTest,
  assertExecution,
  assertExecutionWith,
  initAda,
  initAndAssertAda,
  initAndAssertAdaWith,
  initAndAssertLovelace,
  initLovelace,
  withCollateral,
  withContract,
  withContractAs,
 )
import Test.Plutip.Contract.Types (WalletTag (BaseTag, PkhTag))
import Test.Plutip.Internal.BotPlutusInterface.Lookups (WalletLookups (lookupWallet), lookupAddress)
import Test.Plutip.Internal.BotPlutusInterface.Types (BaseWallet (BaseWallet), PkhWallet (PkhWallet), ValueOrdering (VLt))
import Test.Plutip.Internal.Types (
  FailureReason (CaughtException, ContractExecutionError),
  isException,
 )
import Test.Plutip.LocalCluster (withConfiguredCluster)
import Test.Plutip.Options (TraceOption (ShowBudgets, ShowTraceButOnlyContext))
import Test.Plutip.Predicate (
  assertOverallBudget,
  budgetsFitUnder,
  errorSatisfies,
  failReasonSatisfies,
  overallBudgetFits,
  policyLimit,
  scriptLimit,
  shouldFail,
  shouldSucceed,
  shouldThrow,
  shouldYield,
  stateIs,
  stateSatisfies,
  yieldSatisfies,
 )
import Test.Plutip.Predicate qualified as Predicate
import Test.Tasty (TestTree)

test :: TestTree
test =
  withConfiguredCluster
    def
    "Basic integration: launch, add wallet, tx from wallet to wallet"
    $ [
        -- Basic Succeed or Failed tests
        assertExecution
          "Contract 1"
          (initAda (PkhTag "w1") (100 : replicate 10 7))
          (withContract $ const getUtxos)
          [ shouldSucceed
          , Predicate.not shouldFail
          ]
      , assertExecution
          "Contract 2"
          (initAda (PkhTag "w1") [100])
          (withContract $ const getUtxosThrowsErr)
          [ shouldFail
          , Predicate.not shouldSucceed
          ]
      , assertExecutionWith
          [ShowTraceButOnlyContext ContractLog $ Error [AnyLog]]
          "Contract 3"
          (initAda (PkhTag "w1") [100])
          ( withContract $
              const $ do
                Contract.logInfo @Text "Some contract log with Info level."
                Contract.logDebug @Text "Another contract log with debug level." >> getUtxosThrowsEx
          )
          [ shouldFail
          , Predicate.not shouldSucceed
          ]
      , assertExecution
          "Pay negative amount"
          (initAda (PkhTag "w1") [100])
          ( withContract $ \wl -> do
              PkhWallet pkh1 <- lookupWallet wl (PkhTag "w1")
              payTo pkh1 (-10_000_000)
          )
          [shouldFail]
      , -- Tests with wallet's Value assertions
        assertExecution
          "Pay from wallet to wallet"
          ( initAda (PkhTag "w1") [100]
              <> initAndAssertAda (PkhTag "w2") [100, 13] 123
          )
          ( withContract $ \wl -> do
              PkhWallet pkh1 <- lookupWallet wl (PkhTag "w2")
              payTo pkh1 10_000_000
          )
          [shouldSucceed]
      , assertExecution
          "Two contracts one after another"
          ( initAndAssertAdaWith (PkhTag "w0") [100] VLt 100 -- own wallet (index 0 in wallets lookups)
              <> initAndAssertAdaWith (PkhTag "w1") [100] VLt 100 -- wallet with index 1 in wallets lookups
          )
          ( do
              void $ -- run something prior to the contract which result will be checked
                withContract $ \wl -> do
                  addr1 <- lookupAddress wl "w1"
                  payToPubKeyAddress addr1 10_000_000
              withContractAs "w1" $ -- run contract which result will be checked
                \wl -> do
                  addr0 <- lookupAddress wl "w0"
                  payToPubKeyAddress addr0 10_000_000
          )
          [shouldSucceed]
      , -- Tests with assertions on Contract return value
        assertExecution
          "Initiate wallet and get UTxOs"
          (initAda (BaseTag "") [100])
          (withContract $ const getUtxos)
          [ yieldSatisfies "Returns single UTxO" ((== 1) . Map.size)
          ]
      , let initFunds = 10_000_000
         in assertExecution
              "Should yield own initial Ada"
              (initLovelace (BaseTag "") [toEnum initFunds])
              (withContract $ const ownValue)
              [ shouldYield (lovelaceValueOf $ toEnum initFunds)
              ]
      , -- Tests with assertions on state
        let initFunds = 10_000_000
         in assertExecution
              "Puts own UTxOs Value to state"
              (initLovelace (BaseTag "") [toEnum initFunds])
              (withContract $ const ownValueToState)
              [ stateIs [lovelaceValueOf $ toEnum initFunds]
              , Predicate.not $ stateSatisfies "length > 1" ((> 1) . length)
              ]
      , -- Tests with assertions on failure
        let expectedErr = ConstraintResolutionContractError OwnPubKeyMissing
            isResolutionError = \case
              ConstraintResolutionContractError _ -> True
              _ -> False
         in assertExecution
              ("Contract which throws `" <> show expectedErr <> "`")
              (initAda (BaseTag "") [100])
              (withContract $ const getUtxosThrowsErr)
              [ shouldThrow expectedErr
              , errorSatisfies "Throws resolution error" isResolutionError
              , Predicate.not $ failReasonSatisfies "Throws exception" isException
              ]
      , let checkException = \case
              CaughtException e -> isJust @ErrorCall (fromException e)
              _ -> False
         in assertExecution
              "Contract which throws exception"
              (initAda (PkhTag "") [100])
              (withContract $ const getUtxosThrowsEx)
              [ shouldFail
              , Predicate.not shouldSucceed
              , failReasonSatisfies "Throws ErrorCall" checkException
              ]
      , -- tests with assertions on execution budget
        assertExecutionWith
          [ShowBudgets] -- this influences displaying the budgets only and is not necessary for budget assertions
          "Lock then spend contract"
          (initAda (PkhTag "") (replicate 3 300))
          (withContract $ const lockThenSpend)
          [ shouldSucceed
          , budgetsFitUnder
              (scriptLimit 406250690 1016102)
              (policyLimit 405210181 1019024)
          , assertOverallBudget
              "Assert CPU == 1106851699 and MEM == 2694968"
              (== 1106851699)
              (== 2694968)
          , overallBudgetFits 1106851699 2694968
          ]
      , -- regression tests for time <-> slot conversions
        let isValidityError = \case
              ContractExecutionError e -> "OutsideValidityIntervalUTxO" `isInfixOf` e
              _ -> False
         in assertExecution
              "Fails because outside validity interval"
              (initAda (PkhTag "") [100])
              (withContract $ const failingTimeContract)
              [ shouldFail
              , failReasonSatisfies "Execution error is OutsideValidityIntervalUTxO" isValidityError
              ]
      , assertExecution
          "Passes validation with exact time range checks"
          (initAda (PkhTag "") [100])
          (withContract $ const successTimeContract)
          [shouldSucceed]
      , -- always fail validation test
        let errCheck e = "I always fail" `isInfixOf` pack (show e)
         in assertExecution
              "Always fails to validate"
              (initAda (PkhTag "") [100])
              (withContract $ const lockThenFailToSpend)
              [ shouldFail
              , errorSatisfies "Fail validation with 'I always fail'" errCheck
              ]
      , walletLookupsTest
      , -- Test `adjustUnbalancedTx`
        runAdjustTest
      ]
      ++ testValueAssertionsOrderCorrectness

-- Tests for https://github.com/mlabs-haskell/plutip/issues/84
testValueAssertionsOrderCorrectness :: [ClusterTest]
testValueAssertionsOrderCorrectness =
  [ -- withContract case
    let wallet0 = 100_000_000
        wallet1 = 200_000_000
        wallet2 = 300_000_000

        payFee = 146400
        payTo1Amt = 22_000_000
        payTo2Amt = 33_000_000
        wallet1After = wallet1 + payTo1Amt
        wallet2After = wallet2 + payTo2Amt
        wallet0After =
          wallet0
            - payTo1Amt
            - payFee
            - payTo2Amt
            - payFee
     in assertExecution
          "Values asserted in correct order with withContract"
          ( withCollateral $
              initAndAssertLovelace (PkhTag "w0") [wallet0] wallet0After
                <> initAndAssertLovelace (PkhTag "w1") [wallet1] wallet1After
                <> initAndAssertLovelace (PkhTag "w2") [wallet2] wallet2After
          )
          ( do
              withContract $ \wl -> do
                PkhWallet w1pkh <- lookupWallet wl (PkhTag "w1")
                PkhWallet w2pkh <- lookupWallet wl (PkhTag "w2")
                _ <- payTo w1pkh (toInteger payTo1Amt)
                _ <- waitNSlots 2
                payTo w2pkh (toInteger payTo2Amt)
          )
          [shouldSucceed]
  , -- withContractAs case
    let wallet0 = 100_000_000
        wallet1 = 200_000_000
        wallet2 = 300_000_000

        payFee = 146400
        payTo0Amt = 11_000_000
        payTo1Amt = 22_000_000
        payTo2Amt = 33_000_000

        wallet0After = wallet0 + payTo0Amt
        wallet2After =
          wallet2
            + payTo2Amt
            - payTo1Amt
            - payFee

        wallet1After =
          wallet1
            + payTo1Amt
            - payTo0Amt
            - payFee
            - payTo2Amt
            - payFee
     in assertExecution
          "Values asserted in correct order with withContractAs"
          ( withCollateral $ -- Initialize all the wallets with the collateral utxo.
              initAndAssertLovelace (PkhTag "w0") [wallet0] wallet0After
                <> initAndAssertLovelace (PkhTag "w1") [wallet1] wallet1After
                <> initAndAssertLovelace (PkhTag "w2") [wallet2] wallet2After
          )
          ( do
              void $
                withContractAs "w1" $ \wl -> do
                  PkhWallet w0pkh <- lookupWallet wl (PkhTag "w0")
                  PkhWallet w2pkh <- lookupWallet wl (PkhTag "w2")
                  _ <- payTo w0pkh (toInteger payTo0Amt)
                  _ <- waitNSlots 2
                  payTo w2pkh (toInteger payTo2Amt)

              withContractAs "w2" $ \wl -> do
                PkhWallet w1pkh <- lookupWallet wl (PkhTag "w1")
                payTo w1pkh (toInteger payTo1Amt)
          )
          [shouldSucceed]
  ]

walletLookupsTest :: ClusterTest
walletLookupsTest =
  assertExecution @() @Text
    "Wallets initilized expectedly."
    ( initAndAssertAda (BaseTag "a") [10, 20] 30
        <> initAndAssertAda (BaseTag "b") [11, 22] 33
        <> initAndAssertAda (PkhTag "c") [1, 2] 3
    )
    ( withContract $ \wl -> do
        BaseWallet pkhb spkhb <- lookupWallet wl (BaseTag "b")
        PkhWallet pkhc <- lookupWallet wl (PkhTag "c")
        addrb <- lookupAddress wl "b"
        addrc <- lookupAddress wl "c"

        case addrb of
          Address (PubKeyCredential pkhb') (Just (StakingHash (PubKeyCredential spkhb'))) ->
            if pkhb == PaymentPubKeyHash pkhb' && spkhb == StakePubKeyHash spkhb'
              then pure ()
              else throwError "Unexpected key hashes of wallet 'b'."
          _ -> throwError "Unexpected address of wallet 'b'."

        case addrc of
          Address (PubKeyCredential pkhc') Nothing ->
            if pkhc == PaymentPubKeyHash pkhc'
              then pure ()
              else throwError "Unexpected key hashes of wallet 'c'."
          _ -> throwError "Unexpected address of wallet 'c'."

        ourAddr :| _ <- Contract.ownAddresses
        case ourAddr of
          Address (PubKeyCredential _) (Just (StakingHash (PubKeyCredential _))) -> pure ()
          _ -> throwError "Unexpected contract own address."
    )
    [shouldSucceed]

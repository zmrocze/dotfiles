-- | This module provides some predicates or assertions, that could be used together with
--  `Test.Plutip.Contract.assertExecution` to run tests for Contract in private testnet.
--
--  Module also exports `Predicate` constructor itself, so any arbitrary predicate could be made.
module Test.Plutip.Predicate (
  Predicate (..),
  pTag,
  shouldSucceed,
  Test.Plutip.Predicate.not,
  shouldFail,
  yieldSatisfies,
  shouldYield,
  errorSatisfies,
  failReasonSatisfies,
  shouldThrow,
  stateSatisfies,
  stateIs,
  budgetsFitUnder,
  policyLimit,
  scriptLimit,
  assertOverallBudget,
  overallBudgetFits,
  noBudgetsMessage,
) where

import BotPlutusInterface.Types (TxBudget (TxBudget), mintBudgets, spendBudgets)
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import Data.Map qualified as Map
import Ledger (ExBudget (ExBudget), ExCPU (ExCPU), ExMemory (ExMemory), TxId, Value)
import PlutusCore.Evaluation.Machine.ExMemory (CostingInteger)
import Prettyprinter (
  Doc,
  align,
  defaultLayoutOptions,
  indent,
  layoutPretty,
  viaShow,
  vsep,
  (<+>),
 )
import Prettyprinter.Render.String (renderString)
import Test.Plutip.Internal.Types (
  ExecutionResult (ExecutionResult, contractState, outcome),
  FailureReason (CaughtException, ContractExecutionError),
  budgets,
  isSuccessful,
 )
import Test.Plutip.Tools.Format (fmtExBudget, fmtTxBudgets)
import Text.Show.Pretty (ppShow)


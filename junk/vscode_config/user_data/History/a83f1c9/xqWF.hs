-- | Wallets initiation
module Test.Plutip.Contract.Init (
  initLovelace,
  initLovelaceAssertValueWith,
  initLovelaceAssertValue,
  initAndAssertLovelaceWith,
  initAndAssertLovelace,
  initAda,
  initAdaAssertValueWith,
  initAdaAssertValue,
  initAndAssertAdaWith,
  initAndAssertAda,
  ada,
) where

import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NonEmpty

import Data.Bifunctor (second)

-- import Ledger (Value)
-- import Ledger.Ada qualified as Ada
-- import Ledger.Value qualified as Value

import Numeric.Positive (Positive)

import Test.Plutip.Contract.Types (
  TestWallet (TestWallet, twInitDistribuition),
  TestWallets (TestWallets, unTestWallets),
  ValueOrdering (VEq),
 )

-- | Create a wallet with the given amounts of lovelace.
--  Each amount will be sent to address as separate UTXO.
--
-- @since 0.2
initLovelace :: [Positive] -> TestWallets
initLovelace initial = TestWallets $ TestWallet initial :| []

-- | Create a wallet with the given amounts of Ada.
--
-- @since 0.2
initAda :: [Positive] -> TestWallets
initAda initial = initLovelace (map ada initial)

-- | Create a wallet with the given amounts of Ada, and after contract execution
-- compare the values at the wallet address with the given ordering and ada amount.
--
-- @since 0.2
initAndAssertAdaWith :: [Positive] -> ValueOrdering -> Positive -> TestWallets
initAndAssertAdaWith initial ord expect =
  initAndAssertLovelaceWith (map ada initial) ord (ada expect)

-- | Create a wallet with the given amounts of Ada, and after contract execution
-- check if values at the wallet address are equal to a given ada amount.
--
-- @since 0.2
initAndAssertAda :: [Positive] -> Positive -> TestWallets
initAndAssertAda initial expect =
  initAndAssertLovelace (map ada initial) (ada expect)

-- | Library functions works with amounts in `Lovelace`.
-- This function helps to specify amounts in `Ada` easier.
ada :: Positive -> Positive
ada = (* 1_000_000)

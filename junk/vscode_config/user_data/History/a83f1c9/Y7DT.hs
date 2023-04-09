-- | Wallets initiation
module Test.Plutip.Contract.Init (
  initLovelace,
  initAda,
  ada,
) where

import Data.List.NonEmpty (NonEmpty ((:|)))


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

-- | Library functions works with amounts in `Lovelace`.
-- This function helps to specify amounts in `Ada` easier.
ada :: Positive -> Positive
ada = (* 1_000_000)

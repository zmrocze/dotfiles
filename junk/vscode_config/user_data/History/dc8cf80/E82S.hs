{-# LANGUAGE ConstraintKinds #-}

module Test.Plutip.Contract.Types (
  TestWallets (TestWallets, unTestWallets),
  TestWallet (..),
  compareValuesWith,
  ValueOrdering (..),
) where


import Data.List.NonEmpty (NonEmpty)
-- import Ledger.Value (Value)
-- import Ledger.Value qualified as Value
import Numeric.Positive (Positive)
import Plutus.V1.Ledger.Value (Value)
import qualified Plutus.V1.Ledger.Value as Value
newtype TestWallets = TestWallets {unTestWallets :: NonEmpty TestWallet}
  deriving newtype (Semigroup)

newtype TestWallet = TestWallet
  { twInitDistribuition :: [Positive]  }
  deriving stock (Show)

data ValueOrdering = VEq | VGt | VLt | VGEq | VLEq
  deriving stock (Show)

-- | Value doesn't have an Ord instance, so we cannot use `compare`
compareValuesWith :: ValueOrdering -> Value -> Value -> Bool
compareValuesWith VEq = (==)
compareValuesWith VGt = Value.gt
compareValuesWith VLt = Value.lt
compareValuesWith VGEq = Value.geq
compareValuesWith VLEq = Value.leq

{-# LANGUAGE GADTs #-}

module Test.Plutip.Internal.BotPlutusInterface.Types (
  BpiError (..),
  BpiWallet (BpiWallet, payKeys, stakeKeys, bwTag),
  TestWallet (TestWallet, twInitDistribiution, twExpected, twTag),
  TestWallets (TestWallets, unTestWallets),
  ValueOrdering (VEq, VGt, VLt, VGEq, VLEq),
  compareValuesWith,
  WalletTag (..),
  TestWallet' (..),
  WalletTypeError (..),
  WalletInfo,
  ownPaymentPubKeyHash,
  ownStakePubKeyHash,
  ownAddress,
  getTag,
  -- WalletInfo' (..),
  testWallet',
  SomeBpiWallet (..),
  SomeTestWallet' (..),
  BaseWallet (..),
  PkhWallet (..),
) where

import Control.Lens.Prism (_Right)
import Data.Data (Typeable)
import Data.List.NonEmpty (NonEmpty)
import Ledger (Address, PaymentPubKeyHash, StakePubKeyHash, Value, pubKeyHashAddress)
import Ledger.Value qualified as Value
import Numeric.Positive (Positive)
import Plutus.Contract.Error (AsContractError (_ContractError))
import Test.Plutip.Internal.BotPlutusInterface.Keys (KeyPair, StakeKeyPair)

data WalletTag t k where
  WithStakeKeysTag :: k -> WalletTag BaseWallet k
  EnterpriseTag :: k -> WalletTag PkhWallet k

deriving stock instance Show k => Show (WalletTag t k)
deriving stock instance Eq k => Eq (WalletTag t k)

getTag :: WalletTag t k -> k
getTag = \case
  WithStakeKeysTag k -> k
  EnterpriseTag k -> k

data BpiError
  = SignKeySaveError !String
  | BotInterfaceDirMissing
  deriving stock (Show, Eq)

-- | Wallet that can be used by bot interface,
--  backed by `.skey` file when added to cluster with `addSomeWallet`
data BpiWallet k = BpiWallet
  { payKeys :: KeyPair
  , stakeKeys :: Maybe StakeKeyPair
  , bwTag :: k
  }
  deriving stock (Show)

data SomeBpiWallet = forall k. SomeBpiWallet (BpiWallet k)

newtype TestWallets k = TestWallets {unTestWallets :: NonEmpty (TestWallet' k)}
  deriving newtype (Semigroup)

data TestWallet' k = forall t. TestWallet' (TestWallet t k)

data SomeTestWallet' = forall k. SomeTestWallet' (TestWallet' k)

data TestWallet t k = TestWallet
  { twInitDistribiution :: [Positive]
  , twExpected :: Maybe (ValueOrdering, Value)
  , twTag :: WalletTag t k
  }

testWallet' :: [Positive] -> Maybe (ValueOrdering, Value) -> WalletTag t k -> TestWallet' k
testWallet' twInitDistribiution twExpected twTag = TestWallet' $ TestWallet twInitDistribiution twExpected twTag

data ValueOrdering = VEq | VGt | VLt | VGEq | VLEq

-- | Value doesn't have an Ord instance, so we cannot use `compare`
compareValuesWith :: ValueOrdering -> Value -> Value -> Bool
compareValuesWith VEq = (==)
compareValuesWith VGt = Value.gt
compareValuesWith VLt = Value.lt
compareValuesWith VGEq = Value.geq
compareValuesWith VLEq = Value.leq

data WalletTypeError
  = -- | Expected enterprise address wallet, got one with staking keys.
    ExpectedEnterpriseWallet
  | -- | Expected base address wallet, got one without staking keys.
    ExpectedWalletWithStakeKeys
  | -- | Index outside of range
    BadWalletIndex

instance AsContractError e => AsContractError (Either WalletTypeError e) where
  _ContractError = _Right . _ContractError

instance Show WalletTypeError where
  show ExpectedEnterpriseWallet = "Expected base address wallet, got one with staking keys."
  show ExpectedWalletWithStakeKeys = "Expected base address wallet, got one with staking keys."
  show BadWalletIndex = "Index outside of range."

type WalletInfo = Either BaseWallet PkhWallet

data BaseWallet = BaseWallet {getBasePkh :: PaymentPubKeyHash, getSpkh :: StakePubKeyHash}
  deriving stock (Eq, Show, Typeable)
newtype PkhWallet = PkhWallet {getPkh :: PaymentPubKeyHash}
  deriving stock (Eq, Show, Typeable)

ownPaymentPubKeyHash :: WalletInfo -> PaymentPubKeyHash
ownPaymentPubKeyHash = either getBasePkh getPkh

ownStakePubKeyHash :: WalletInfo -> Maybe StakePubKeyHash
ownStakePubKeyHash = either (Just . getSpkh) (const Nothing)

ownAddress :: WalletInfo -> Address
ownAddress w = pubKeyHashAddress (ownPaymentPubKeyHash w) (ownStakePubKeyHash w)

module Spec.TestContract.SimpleContracts (
  getUtxos,
  getUtxosThrowsErr,
  getUtxosThrowsEx,
  payTo,
  ownValue,
  ownValueToState,
) where

import Plutus.Contract (
  Contract,
  ContractError (ConstraintResolutionContractError),
  submitTx,
  utxosAt,
 )
import Plutus.Contract qualified as Contract

import Ledger (
  CardanoTx,
  ChainIndexTxOut,
  PaymentPubKeyHash,
  TxOutRef,
  Value,
  ciTxOutValue,
  getCardanoTxId,
 )

import Data.Map (Map)
import Ledger.Ada qualified as Ada

import Control.Lens ((^.))
import Control.Monad (void)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text (Text)
import Ledger.Constraints (MkTxError (OwnPubKeyMissing))
import Ledger.Constraints qualified as Constraints
import Plutus.PAB.Effects.Contract.Builtin (EmptySchema)

getUtxos :: Contract [Value] EmptySchema Text (Map TxOutRef ChainIndexTxOut)
getUtxos = do
  addr <- NonEmpty.head <$> Contract.ownAddresses
  utxosAt addr

getUtxosThrowsErr :: Contract () EmptySchema ContractError (Map TxOutRef ChainIndexTxOut)
getUtxosThrowsErr =
  Contract.throwError $ ConstraintResolutionContractError OwnPubKeyMissing

getUtxosThrowsEx :: Contract () EmptySchema Text (Map TxOutRef ChainIndexTxOut)
getUtxosThrowsEx = error "This Exception was thrown intentionally in Contract.\n"

payTo :: PaymentPubKeyHash -> Integer -> Contract () EmptySchema Text CardanoTx
payTo toPkh amt = do
  tx <- submitTx (Constraints.mustPayToPubKey toPkh (Ada.lovelaceValueOf amt))
  _ <- Contract.awaitTxConfirmed (getCardanoTxId tx)
  pure tx

ownValue :: Contract [Value] EmptySchema Text Value
ownValue = foldMap (^. ciTxOutValue) <$> getUtxos

-- this Contract fails, but state should change in expected way
ownValueToState :: Contract [Value] EmptySchema Text ()
ownValueToState = do
  ownValue >>= Contract.tell . (: [])
  void $ Contract.throwError "Intentional fail"
  ownValue
    >>= Contract.tell
      . (: []) -- should not be in state

-- | This module demonstrates how the `Contract` interface can be used to build,
-- | balance, and submit a smart-contract transaction. It creates a transaction
-- | that pays two Ada to the `AlwaysSucceeds` script address.
module Ctl.Examples.AlwaysSucceeds
  ( alwaysSucceeds
  , alwaysSucceedsScript
  , contract
  , example
  , main
  , payToAlwaysSucceeds
  , spendFromAlwaysSucceeds
  ) where

import Contract.Prelude

import Contract.Address (ownStakePubKeysHashes, scriptHashAddress)
import Contract.Config (ConfigParams, testnetNamiConfig)
import Contract.Credential (Credential(PubKeyCredential))
import Contract.Log (logInfo')
import Contract.Monad (Contract, launchAff_, runContract)
import Contract.PlutusData (PlutusData, unitDatum, unitRedeemer)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (Validator(Validator), ValidatorHash, validatorHash)
import Contract.TextEnvelope (decodeTextEnvelope, plutusScriptV1FromEnvelope)
import Contract.Transaction
  ( TransactionHash
  , _input
  , awaitTxConfirmed
  , lookupTxHash
  )
import Contract.TxConstraints (TxConstraints)
import Contract.TxConstraints as Constraints
import Contract.Utxos (utxosAt)
import Contract.Value as Value
import Control.Monad.Error.Class (liftMaybe)
import Ctl.Examples.Helpers (buildBalanceSignAndSubmitTx) as Helpers
import Data.Array (head)
import Data.BigInt as BigInt
import Data.Lens (view)
import Effect.Exception (error)

main :: Effect Unit
main = example testnetNamiConfig

contract :: Contract () Unit
contract = do
  logInfo' "Running Examples.AlwaysSucceeds"
  validator <- alwaysSucceedsScript
  let vhash = validatorHash validator
  logInfo' "Attempt to lock value"
  txId <- payToAlwaysSucceeds vhash
  awaitTxConfirmed txId
  logInfo' "Tx submitted successfully, Try to spend locked values"
  spendFromAlwaysSucceeds vhash validator txId

example :: ConfigParams () -> Effect Unit
example cfg = launchAff_ do
  runContract cfg contract

payToAlwaysSucceeds :: ValidatorHash -> Contract () TransactionHash
payToAlwaysSucceeds vhash = do
  -- Send to own stake credential. This is used to test mustPayToScriptAddress.
  mbStakeKeyHash <- join <<< head <$> ownStakePubKeysHashes
  let
    constraints :: TxConstraints Unit Unit
    constraints =
      case mbStakeKeyHash of
        Nothing ->
          Constraints.mustPayToScript vhash unitDatum
            Constraints.DatumWitness
            $ Value.lovelaceValueOf
            $ BigInt.fromInt 2_000_000
        Just stakeKeyHash ->
          Constraints.mustPayToScriptAddress vhash
            (PubKeyCredential $ unwrap stakeKeyHash)
            unitDatum
            Constraints.DatumWitness
            $ Value.lovelaceValueOf
            $ BigInt.fromInt 2_000_000

    lookups :: Lookups.ScriptLookups PlutusData
    lookups = mempty

  Helpers.buildBalanceSignAndSubmitTx lookups constraints

spendFromAlwaysSucceeds
  :: ValidatorHash
  -> Validator
  -> TransactionHash
  -> Contract () Unit
spendFromAlwaysSucceeds vhash validator txId = do
  -- Use own stake credential if available
  mbStakeKeyHash <- join <<< head <$> ownStakePubKeysHashes
  let
    scriptAddress =
      scriptHashAddress vhash (PubKeyCredential <<< unwrap <$> mbStakeKeyHash)
  utxos <- utxosAt scriptAddress
  txInput <-
    liftM
      ( error
          ( "The id "
              <> show txId
              <> " does not have output locked at: "
              <> show scriptAddress
          )
      )
      (view _input <$> head (lookupTxHash txId utxos))
  let
    lookups :: Lookups.ScriptLookups PlutusData
    lookups = Lookups.validator validator
      <> Lookups.unspentOutputs utxos

    constraints :: TxConstraints Unit Unit
    constraints =
      Constraints.mustSpendScriptOutput txInput unitRedeemer
  spendTxId <- Helpers.buildBalanceSignAndSubmitTx lookups constraints
  awaitTxConfirmed spendTxId
  logInfo' "Successfully spent locked values."

foreign import alwaysSucceeds :: String

alwaysSucceedsScript :: Contract () Validator
alwaysSucceedsScript =
  liftMaybe (error "Error decoding alwaysSucceeds") do
    envelope <- decodeTextEnvelope alwaysSucceeds
    Validator <$> plutusScriptV1FromEnvelope envelope

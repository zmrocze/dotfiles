module Internal.Plutus.Types.ScriptContext.V1.TxInfo where

import Contract.Address (Address)
import Contract.Prelude (type (/\), Maybe)
import Contract.Value (Value)
import Ctl.Internal.Plutus.Types.Credential (StakingCredential)
import Ctl.Internal.Serialization.Types (BigInt, TransactionHash, TransactionInput)
import Ctl.Internal.Types.Datum (DataHash)
import Ctl.Internal.Types.Interval (POSIXTimeRange)
import Ctl.Internal.Types.PlutusData (PlutusData)
import Ctl.Internal.Types.PubKeyHash (PubKeyHash)

-- a V1 TransactionOutput - no inline datums and reference scripts
data TransactionOutputV1 = TxOut {
    txOutAddress   :: Address
  , txOutValue     :: Value
  , txOutDatumHash :: Maybe DataHash
}

-- | A V1 input of a pending transaction.
data TxInInfoV1 = TxInInfoV1
  { txInInfoOutRef   :: TransactionInput -- TxOutRef
  , txInInfoResolved :: TransactionOutputV1
  }

-- https://github.com/input-output-hk/plutus/blob/c8d4364d0e639fef4d5b93f7d6c0912d992b54f9/plutus-ledger-api/src/PlutusLedgerApi/V1/DCert.hs#L24
data DCert
  = DCertDelegRegKey StakingCredential
  | DCertDelegDeRegKey StakingCredential
  | DCertDelegDelegate
      StakingCredential
      -- ^ delegator
      PubKeyHash
      -- ^ delegatee
  | -- | A digest of the PoolParams
    DCertPoolRegister
      PubKeyHash
      -- ^ poolId
      PubKeyHash
      -- ^ pool VFR
  | -- | The retiremant certificate and the Epoch N
    DCertPoolRetire PubKeyHash BigInt -- NB: Should be Word64 but we only have Integer on-chain
  | -- | A really terse Digest
    DCertGenesis
  | -- | Another really terse Digest
    DCertMir

-- | A pending transaction. This is the view as seen by validator scripts, so some details are stripped out.
-- For plutus V1.
data TxInfoV1 = TxInfoV1
  { txInfoInputs      :: Array TxInInfoV1 -- ^ Transaction inputs
  , txInfoOutputs     :: Array TransactionOutputV1 -- ^ Transaction outputs
  , txInfoFee         :: Value -- ^ The fee paid by this transaction.
  , txInfoMint        :: Value -- ^ The 'Value' minted by this transaction.
  , txInfoDCert       :: Array DCert -- ^ Digests of certificates included in this transaction
  , txInfoWdrl        :: Array (StakingCredential /\ BigInt) -- ^ Withdrawals
  , txInfoValidRange  :: POSIXTimeRange -- ^ The valid range for the transaction.
  , txInfoSignatories :: Array PubKeyHash -- ^ Signatures provided with the transaction, attested that they all signed the tx
  , txInfoData        :: Array (DataHash /\ PlutusData)
  , txInfoId          :: TransactionHash
  -- ^ Hash of the pending transaction (excluding witnesses)
  }

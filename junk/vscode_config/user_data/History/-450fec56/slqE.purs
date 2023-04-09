module Internal.Plutus.Types.ScriptContext.V1.TxInfo where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)

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
newtype TxInInfoV1 = TxInInfoV1
  { txInInfoOutRef   :: TransactionInput -- TxOutRef
  , txInInfoResolved :: TransactionOutputV1
  }

derive instance Eq TxInInfoV1
derive instance Ord TxInInfoV1
derive instance Newtype TxInInfoV1 _
derive instance Generic TxInInfoV1 _

instance Show TxInInfoV1 where 
  show = genericShow


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

derive instance Eq DCert
derive instance Ord DCert
derive instance Generic DCert _

instance Show DCert where 
  show = genericShow


-- | A pending transaction. This is the view as seen by validator scripts, so some details are stripped out.
-- For plutus V1.
newtype TxInfoV1 = TxInfoV1
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

derive instance Eq TxInfoV1
derive instance Ord TxInfoV1
derive instance Newtype TxInfoV1 _
derive instance Generic TxInfoV1 _

instance Show TxInfoV1 where 
  show = genericShow

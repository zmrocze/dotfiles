module Internal.Plutus.Types.ScriptContext.V2.TxInfo where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)

import Ctl.Internal.Cardano.Types.Value (CurrencySymbol)
import Ctl.Internal.Plutus.Types.AssocMap (Map)
import Ctl.Internal.Plutus.Types.Credential (StakingCredential)
import Ctl.Internal.Plutus.Types.Value (Value)
-- import Ctl.Internal.Serialization.Types (BigInt, DataHash, TransactionHash, TransactionInput, TransactionOutput)
import Ctl.Internal.Types.Interval (POSIXTimeRange)
import Ctl.Internal.Types.PlutusData (PlutusData)
import Ctl.Internal.Types.PubKeyHash (PubKeyHash)
import Internal.Plutus.Types.ScriptContext.V1.TxInfo (DCert)

-- | A V2 input of a pending transaction.
data TxInInfoV2 = TxInInfoV2
  { txInInfoOutRef   :: TransactionInput
  , txInInfoResolved :: TransactionOutput
  }

-- | Purpose of the script that is currently running
data ScriptPurpose
  = Minting CurrencySymbol
  | Spending TransactionInput
  | Rewarding StakingCredential
  | Certifying DCert

-- | A pending transaction. This is the view as seen by validator scripts, so some details are stripped out.
newtype TxInfoV2 = TxInfoV2
  { txInfoInputs          :: Array TxInInfoV2 -- ^ Transaction inputs
  , txInfoReferenceInputs :: Array TxInInfoV2 -- ^ Transaction reference inputs
  , txInfoOutputs         :: Array TransactionOutput -- ^ Transaction outputs
  , txInfoFee             :: Value -- ^ The fee paid by this transaction.
  , txInfoMint            :: Value -- ^ The 'Value' minted by this transaction.
  , txInfoDCert           :: Array DCert -- ^ Digests of certificates included in this transaction
  , txInfoWdrl            :: Map StakingCredential BigInt -- ^ Withdrawals
  , txInfoValidRange      :: POSIXTimeRange -- ^ The valid range for the transaction.
  , txInfoSignatories     :: Array PubKeyHash -- ^ Signatures provided with the transaction, attested that they all signed the tx
  , txInfoRedeemers       :: Map ScriptPurpose PlutusData
  , txInfoData            :: Map DataHash PlutusData
  , txInfoId              :: TransactionHash
  -- ^ Hash of the pending transaction (excluding witnesses)
  }

derive instance Eq TxInfoV2
derive instance Newtype TxInfoV2 _
derive instance Generic TxInfoV2 _

instance Show TxInfoV2 where 
  show = genericShow
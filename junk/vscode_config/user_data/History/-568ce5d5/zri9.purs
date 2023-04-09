module Internal.Plutus.Types.ScriptContext.V2.TxInfo where

import Prelude

import Ctl.Internal.Cardano.Types.Transaction (Redeemer)
import Ctl.Internal.Cardano.Types.Value (CurrencySymbol)
import Ctl.Internal.Plutus.Types.AssocMap (Map)
import Ctl.Internal.Plutus.Types.Credential (StakingCredential)
import Ctl.Internal.Plutus.Types.Transaction (TransactionOutput)
import Ctl.Internal.Plutus.Types.Value (Value)
import Ctl.Internal.ToData (class ToData, genericToData, toData)
import Ctl.Internal.Types.BigNum (fromInt)
import Ctl.Internal.Types.Datum (Datum)
import Ctl.Internal.Types.Interval (POSIXTimeRange)
import Ctl.Internal.Types.PlutusData (PlutusData(..))
import Ctl.Internal.Types.PubKeyHash (PubKeyHash)
import Ctl.Internal.Types.Transaction (DataHash, TransactionHash, TransactionInput)
import Data.BigInt (BigInt)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Internal.Plutus.Types.ScriptContext.V1.TxInfo (DCert)

-- | A V2 input of a pending transaction.
newtype TxInInfoV2 = TxInInfoV2
  { txInInfoOutRef   :: TransactionInput
  , txInInfoResolved :: TransactionOutput
  }

derive instance Eq TxInInfoV2
derive instance Newtype TxInInfoV2 _
derive instance Generic TxInInfoV2 _

instance Show TxInInfoV2 where 
  show = genericShow

-- | Purpose of the script that is currently running
data ScriptPurpose
  = Minting CurrencySymbol
  | Spending TransactionInput
  | Rewarding StakingCredential
  | Certifying DCert

derive instance Eq ScriptPurpose
derive instance Generic ScriptPurpose _

instance Show ScriptPurpose where 
  show = genericShow

instance ToData ScriptPurpose where 
  toData x = case x of
    Minting sym -> constr 0 sym
    Spending out -> constr 0 out
    Minting sym -> constr 0 sym
    Minting sym -> constr 0 sym
    where 
      constr n x = Constr (fromInt n) [toData x]

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
  , txInfoRedeemers       :: Map ScriptPurpose Redeemer
  , txInfoData            :: Map DataHash Datum
  , txInfoId              :: TransactionHash
  -- ^ Hash of the pending transaction (excluding witnesses)
  }

derive instance Eq TxInfoV2
derive instance Newtype TxInfoV2 _
derive instance Generic TxInfoV2 _

instance Show TxInfoV2 where 
  show = genericShow
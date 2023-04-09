module Internal.Plutus.Types.ScriptContext.V2.TxInfo where

import Ctl.Internal.Plutus.Types.AssocMap (Map)
import Ctl.Internal.Plutus.Types.Credential (StakingCredential)
import Ctl.Internal.Plutus.Types.Value (Value)
import Ctl.Internal.Serialization.Types (BigInt, TransactionInput, TransactionOutput)
import Ctl.Internal.Types.Interval (POSIXTimeRange)
import Ctl.Internal.Types.PubKeyHash (PubKeyHash(..))
import Internal.Plutus.Types.ScriptContext.V1.TxInfo (DCert)

-- | A V2 input of a pending transaction.
data TxInInfo = TxInInfo
    { txInInfoOutRef   :: TransactionInput
    , txInInfoResolved :: TransactionOutput
    }

-- | Purpose of the script that is currently running
data ScriptPurpose
    = Minting CurrencySymbol
    | Spending TxOutRef
    | Rewarding StakingCredential
    | Certifying DCert

-- | A pending transaction. This is the view as seen by validator scripts, so some details are stripped out.
data TxInfo = TxInfo
    { txInfoInputs          :: Array TxInInfo -- ^ Transaction inputs
    , txInfoReferenceInputs :: Array TxInInfo -- ^ Transaction reference inputs
    , txInfoOutputs         :: Array TransactionOutput -- ^ Transaction outputs
    , txInfoFee             :: Value -- ^ The fee paid by this transaction.
    , txInfoMint            :: Value -- ^ The 'Value' minted by this transaction.
    , txInfoDCert           :: Array DCert -- ^ Digests of certificates included in this transaction
    , txInfoWdrl            :: Map StakingCredential BigInt -- ^ Withdrawals
    , txInfoValidRange      :: POSIXTimeRange -- ^ The valid range for the transaction.
    , txInfoSignatories     :: Array PubKeyHash -- ^ Signatures provided with the transaction, attested that they all signed the tx
    , txInfoRedeemers       :: Map ScriptPurpose Redeemer
    , txInfoData            :: Map DatumHash Datum
    , txInfoId              :: TxId
    -- ^ Hash of the pending transaction (excluding witnesses)
    }

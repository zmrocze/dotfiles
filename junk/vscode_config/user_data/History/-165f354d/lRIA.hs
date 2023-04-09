module ExampleScriptContext where
import Ledger (ScriptContext(ScriptContext), unitRedeemer, TxInInfo (TxInInfo), TxOutRef (..), TxId (TxId), TxOut (TxOut), Address (..), always, interval, fromMilliSeconds, Datum (Datum), TxInfo (..))
import Ledger.Value as Value
import Data.Void
import Ledger.Constraints (mustMintValueWithRedeemer, mustPayToPubKey, plutusV1MintingPolicy)
import qualified Data.List.NonEmpty as NonEmpty
import Plutus.Contract (ownAddresses, ownPaymentPubKeyHash, ownFirstPaymentPubKeyHash, ContractError)
import Ledger.Constraints qualified as Constraints
import Ledger.Scripts qualified as Scripts
import Ledger.Typed.Scripts (mkUntypedMintingPolicy)
import Ledger.Typed.Scripts qualified as TypedScripts
import Ledger.Value (tokenName)
import Plutus.Contract (Contract, adjustUnbalancedTx, awaitTxConfirmed, mkTxConstraints, submitTxConfirmed, submitTxConstraintsWith)
import qualified PlutusTx.Prelude as PP
import qualified PlutusTx
import qualified Ledger.Scripts as ScriptUtils
import Ledger.Ada (adaValueOf)
import Plutus.V2.Ledger.Contexts (TxInfo)
import Plutus.V1.Ledger.Api (StakingCredential(..), Credential (..), ToData (toBuiltinData), ScriptPurpose (Spending), toData)
import Plutus.V2.Ledger.Api qualified as V2

addr1 = Address pubkCred Nothing 
addr2 = Address scriptCred (Just stakingCred) 
value0 = mempty :: Value
value1 = singleton adaSymbol adaToken 1_000_000
value2 = value1 <> singleton (currencySymbol "1234") (tokenName "abcd") 3
txout1 = TxOut addr1 value1 Nothing
txout2 = TxOut addr2 value2 (Just "def2")
pubkCred = PubKeyCredential "123456"
scriptCred = ScriptCredential "12ab"
stakingCred = StakingHash pubkCred
timerange1 = interval (fromMilliSeconds 13) (fromMilliSeconds 200)
timerange2 = always

txinfoV1 = TxInfo
    { txInfoInputs      = [TxInInfo (TxOutRef (TxId "bbbb") 0) txout1 ] -- ^ Transaction inputs
    , txInfoOutputs     = [txout2] -- ^ Transaction outputs
    , txInfoFee         = value1 -- ^ The fee paid by this transaction.
    , txInfoMint        = value0 -- ^ The 'Value' minted by this transaction.
    , txInfoDCert       = [] -- ^ Digests of certificates included in this transaction
    , txInfoWdrl        = [(stakingCred, 10)] -- ^ Withdrawals
    , txInfoValidRange  = timerange1 -- ^ The valid range for the transaction.
    , txInfoSignatories = ["a420"] -- ^ Signatures provided with the transaction, attested that they all signed the tx
    , txInfoData        = [("fa3f", Datum (toBuiltinData [1 :: Integer, 2]))]
    , txInfoId          = TxId "aa"
    -- ^ Hash of the pending transaction (excluding witnesses)
    }

addr1V2 = Address pubkCred Nothing 
addr2V2 = Address scriptCred (Just stakingCred) 
value0V2 = mempty :: Value
value1V2 = singleton adaSymbol adaToken 1_000_000
value2V2 = value1 <> singleton (currencySymbol "1234") (tokenName "abcd") 3
txout1V2 = V2.TxOut addr1 value1 V2.NoOutputDatum Nothing
txout2V2 = V2.TxOut addr2 value2 (V2.OutputDatumHash "def2") (Just "aa")
pubkCredV2 = PubKeyCredential "123456"
scriptCredV2 = ScriptCredential "12ab"
stakingCredV2 = StakingHash pubkCred
timerange1V2 = interval (fromMilliSeconds 13) (fromMilliSeconds 200)
timerange2V2 = always

txinfoV2 = V2.TxInfo
    { V2.txInfoInputs      = [V2.TxInInfo (TxOutRef (TxId "bbbb") 0) txout1V2 ] -- ^ Transaction inputs
    , V2.txInfoOutputs     = [txout1V2] -- ^ Transaction outputs
    , V2.txInfoReferenceInputs = []
    , V2.txInfoRedeemers   = mempty
    , V2.txInfoFee         = value1 -- ^ The fee paid by this transaction.
    , V2.txInfoMint        = value0 -- ^ The 'Value' minted by this transaction.
    , V2.txInfoDCert       = [] -- ^ Digests of certificates included in this transaction
    , V2.txInfoWdrl        = V2.fromList [(stakingCred, 10)] -- ^ Withdrawals
    , V2.txInfoValidRange  = timerange1 -- ^ The valid range for the transaction.
    , V2.txInfoSignatories = ["a420"] -- ^ Signatures provided with the transaction, attested that they all signed the tx
    , V2.txInfoData        = V2.fromList [("fa3f", Datum (toBuiltinData [1 :: Integer, 2]))]
    , V2.txInfoId          = TxId "aa"
    -- ^ Hash of the pending transaction (excluding witnesses)
    }

context = ScriptContext txinfo (Spending (TxOutRef (TxId "cccc") 0))

contextData :: PlutusTx.Data
contextData = toData context  
module ExampleScriptContext where
import Ledger (ScriptContext(ScriptContext), unitRedeemer, TxInfo (..), TxInInfo (TxInInfo), TxOutRef (..), TxId (TxId), TxOut (TxOut), Address (..), always, interval, fromMilliSeconds, Datum (Datum))
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
import Plutus.V1.Ledger.Api (StakingCredential(..), Credential (..), ToData (toBuiltinData))

addr1 = Address "addres" Nothing 
addr2 = Address "addres" (Just "stakyy") 
value0 = mempty :: Value
value1 = singleton adaSymbol adaToken 1_000_000
value2 = value1 <> singleton (currencySymbol "symbol") (tokenName "name") 3
txout1 = TxOut addr1 value1 Nothing
txout2 = TxOut addr2 value2 (Just "hash")
pubkCred = PubKeyCredential "hash"
stakingCred = StakingHash pubkCred
timerange1 = interval (fromMilliSeconds 13) (fromMilliSeconds 200)
timerange2 = always

txinfo =  TxInfo
    { txInfoInputs      = [TxInInfo (TxOutRef (TxId "byte") 0) txout1 ] -- ^ Transaction inputs
    , txInfoOutputs     = [txout2] -- ^ Transaction outputs
    , txInfoFee         = value1 -- ^ The fee paid by this transaction.
    , txInfoMint        = value0 -- ^ The 'Value' minted by this transaction.
    , txInfoDCert       = [] -- ^ Digests of certificates included in this transaction
    , txInfoWdrl        = [(stakingCred, 10)] -- ^ Withdrawals
    , txInfoValidRange  = timerange1 -- ^ The valid range for the transaction.
    , txInfoSignatories = ["pkhash"] -- ^ Signatures provided with the transaction, attested that they all signed the tx
    , txInfoData        = [("datahash", Datum (toBuiltinData [1, 2]))]
    , txInfoId          = TxId "txid"
    -- ^ Hash of the pending transaction (excluding witnesses)
    }

-- context = ScriptContext txinfo (Spending TxOutRef)
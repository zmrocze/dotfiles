module Internal.Plutus.Types.ScriptContext where

import Internal.Plutus.Types.ScriptContext.V1.TxInfo as V1
import Internal.Plutus.Types.ScriptContext.V2.TxInfo as V2

data TxInfo
  = V1 V1.TxInfoV1
  | V2 V2.TxInfoV2

-- Transaction context shown to validators.
data ScriptContext  = ScriptContext {
  scriptContextTxInfo :: Either V1.TxInfo V2.TxInfo, 
  scriptContextPurpose :: ScriptPurpose 
}

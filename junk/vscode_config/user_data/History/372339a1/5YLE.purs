module Internal.Plutus.Types.ScriptContext where

data TxInfo
  = V1 V1.TxInfo
  | V2 V2.TxInfo

-- Transaction context shown to validators.
data ScriptContext  = ScriptContext {
  scriptContextTxInfo :: Either V1.TxInfo V2.TxInfo, 
  scriptContextPurpose :: ScriptPurpose 
}

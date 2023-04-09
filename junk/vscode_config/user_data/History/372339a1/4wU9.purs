module Internal.Plutus.Types.ScriptContext where



-- Transaction context shown to validators.
data ScriptContext  = ScriptContext {
  scriptContextTxInfo :: Either V1.TxInfo V2.TxInfo, 
  scriptContextPurpose :: ScriptPurpose }
}
module Internal.Plutus.Types.ScriptContext where

-- Transaction context shown to validators.
data ScriptContext  = ScriptContext {
  scriptContextTxInfo :: TxInfo, scriptContextPurpose :: ScriptPurpose }
}
## How to serialize Plutus scripts

### PlutusTx
Take `alwaysSucceeds` as an example: 
```haskell
alwaysSucceeds :: BuiltinData -> BuiltinData -> ()
alwaysSucceeds _ _ = ()

alwaysSucceedsCompiled :: CompiledCode (BuiltinData -> BuiltinData -> ())
alwaysSucceedsCompiled = $$(PlutusTx.compile [|| alwaysSucceeds ||])
```
For older plutus commits use:
```haskell
import Plutus.V1.Ledger.Api (fromCompiledCode)

scriptToCBOR :: CompiledCode a -> ByteString
scriptToCBOR = B.toStrict . serialise . fromCompiledCode
```


main =
  case toCardanoApiScript alwaysSucceedsCompiled of
    Left err -> print err
    Right script ->
      writeFileTextEnvelope @(Script PlutusScriptV1) "always-succeeds.plutus" Nothing script
      >>= either print return
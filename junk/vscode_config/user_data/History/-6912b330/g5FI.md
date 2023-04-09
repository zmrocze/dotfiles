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
import Codec.Serialise (serialise)

scriptToCBOR :: CompiledCode a -> ByteString
scriptToCBOR = B.toStrict . serialise . fromCompiledCode
```
and for newer:
```
import PlutusLedgerApi.Common (serialiseCompiledCode)
import Data.ByteString.Short (fromShort)


```
```haskell
import Cardano.Api (writeFileTextEnvelope, PlutusScriptV1, SerialiseAsCBOR (deserialiseFromCBOR), AsType (AsScript, AsPlutusScriptV1), Script)
```

main =
  case toCardanoApiScript alwaysSucceedsCompiled of
    Left err -> print err
    Right script ->
      writeFileTextEnvelope @(Script PlutusScriptV1) "always-succeeds.plutus" Nothing script
      >>= either print return
## How to serialize Plutus scripts

### PlutusTx
Take `alwaysSucceeds` as an example: 
```haskell
alwaysSucceeds :: BuiltinData -> BuiltinData -> BuiltinData -> ()
alwaysSucceeds _ _ _ = ()

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
and for newer (match on plutus-ledger-api import):
```haskell
import PlutusLedgerApi.Common (serialiseCompiledCode)
import Data.ByteString.Short (fromShort)

scriptToCBOR = Short.fromShort . serialiseCompiledCode
```
Then save a script to a file:
```haskell
import Cardano.Api (writeFileTextEnvelope, PlutusScriptV1, SerialiseAsCBOR (deserialiseFromCBOR), AsType (AsScript, AsPlutusScriptV1), Script)


```

main =
  case toCardanoApiScript alwaysSucceedsCompiled of
    Left err -> print err
    Right script ->
      writeFileTextEnvelope @(Script PlutusScriptV1) "always-succeeds.plutus" Nothing script
      >>= either print return
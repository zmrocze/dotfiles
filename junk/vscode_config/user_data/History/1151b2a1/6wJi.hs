-- {-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:dump-uplc #-}
module Main where


import Ledger (fromCompiledCode, Redeemer, Datum, ScriptContext, Validator)
import Plutus.V1.Ledger.Api (BuiltinData, LedgerBytes (LedgerBytes), toBuiltin)
import PlutusTx (compile, CompiledCode)
import Codec.Serialise (serialise)
import Cardano.Api (writeFileTextEnvelope, PlutusScriptV1, SerialiseAsCBOR (deserialiseFromCBOR), AsType (AsScript, AsPlutusScriptV1), Script)
import qualified Data.ByteString.Lazy as B
import Cardano.Binary (DecoderError)
import qualified Data.ByteString.Short as SBS
import Flat
import Data.ByteString (ByteString)
import Prelude
import Data.Either
import qualified Ledger.Typed.Scripts as Scripts
import Ledger.Typed.Scripts (TypedValidator, ValidatorTypes)

-- import PlutusLedgerApi

{-# INLINABLE alwaysSucceeds #-}
alwaysSucceeds :: BuiltinData -> BuiltinData -> ()
alwaysSucceeds _ _ = ()

-- alwaysSucceedsCompiled :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
alwaysSucceedsCompiled :: CompiledCode (BuiltinData -> BuiltinData -> ())
alwaysSucceedsCompiled = $$(PlutusTx.compile [|| alwaysSucceeds ||])

toCardanoApiScript :: CompiledCode a -> ByteString
toCardanoApiScript = B.toStrict . serialise . fromCompiledCode

-- toCardanoApiScript :: CompiledCode a -> Either DecoderError (Script PlutusScriptV1)
-- toCardanoApiScript = deserialiseFromCBOR (AsScript AsPlutusScriptV1) . B.toStrict . serialise . fromCompiledCode


main = do
  let scriptBytes = toCardanoApiScript alwaysSucceedsCompiled 
  case deserialiseFromCBOR (AsScript AsPlutusScriptV1) scriptBytes of
    Left err -> print err
    Right script ->
      -- writeFileTextEnvelope @(Script PlutusScriptV1) "always-succeeds.plutus" (Just "My little script") script
      writeFileTextEnvelope @(Script PlutusScriptV1) "always-succeeds.plutus" Nothing script
      >>= either print return


{-# INLINEABLE mkValidator #-}
mkValidator :: () -> () -> ScriptContext -> Bool
mkValidator _ _ _ = traceError "I always fail"

data AlwaysFail

instance ValidatorTypes AlwaysFail where
  type DatumType AlwaysFail = ()
  type RedeemerType AlwaysFail = ()

typedValidator :: TypedValidator AlwaysFail
typedValidator =
  Scripts.mkTypedValidator @AlwaysFail
    $$(PlutusTx.compile [||mkValidator||])
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.mkUntypedValidator @() @()

validator :: Validator
validator = Scripts.validatorScript typedValidator

-- {-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:dump-uplc #-}
module Main where


import Ledger (fromCompiledCode, Redeemer, Datum, ScriptContext)
import Plutus.V1.Ledger.Api (BuiltinData, LedgerBytes (LedgerBytes), toBuiltin)
import PlutusTx (compile, CompiledCode)
import Codec.Serialise (serialise)
import Cardano.Api (writeFileTextEnvelope, PlutusScriptV1, SerialiseAsCBOR (deserialiseFromCBOR), AsType (AsScript, AsPlutusScriptV1), Script)
import qualified Data.ByteString.Lazy as B
import Cardano.Binary (DecoderError)
import Prelude

alwaysSucceeds :: BuiltinData -> BuiltinData -> ()
alwaysSucceeds _ _ = ()

alwaysSucceedsCompiled :: CompiledCode (BuiltinData -> BuiltinData -> ())
alwaysSucceedsCompiled = $$(PlutusTx.compile [|| alwaysSucceeds ||])

toCardanoApiScript :: CompiledCode a -> Either DecoderError (Script PlutusScriptV1)
toCardanoApiScript = deserialiseFromCBOR (AsScript AsPlutusScriptV1) . B.toStrict . serialise . fromCompiledCode

main =
  case toCardanoApiScript alwaysSucceedsCompiled of
    Left err -> print err
    Right script ->
      writeFileTextEnvelope @(Script PlutusScriptV1) "always-succeeds.plutus" Nothing script
      >>= either print return

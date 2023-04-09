-- {-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:dump-uplc #-}
module Main where


import Ledger (fromCompiledCode)
import PlutusTx (compile, CompiledCode, BuiltinData)
import Codec.Serialise (serialise)
import Cardano.Api (writeFileTextEnvelope, PlutusScriptV1, SerialiseAsCBOR (deserialiseFromCBOR), AsType (AsScript, AsPlutusScriptV1), Script)
import qualified Data.ByteString.Lazy as B
import Cardano.Binary (DecoderError)
import Prelude

alwaysSucceeds :: BuiltinData -> BuiltinData -> ()
alwaysSucceeds _ _ = ()

alwaysSucceedsCompiled :: CompiledCode (BuiltinData -> BuiltinData -> ())
alwaysSucceedsCompiled = $$(PlutusTx.compile [|| alwaysSucceeds ||])

-- toCardanoApiScript :: CompiledCode a -> Either DecoderError (Script PlutusScriptV1)
toCardanoApiScript = B.toStrict . serialise . fromCompiledCode

deserialiseFromCBOR (AsScript AsPlutusScriptV1)

main =
  case toCardanoApiScript alwaysSucceedsCompiled of
    Left err -> print err
    Right script ->
      writeFileTextEnvelope @(Script PlutusScriptV1) "always-succeeds.plutus" Nothing script
      >>= either print return

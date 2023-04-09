-- {-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:dump-uplc #-}
module Main where


import Ledger (fromCompiledCode, Redeemer, Datum, ScriptContext)
-- import PlutusLedgerApi (BuiltinData, LedgerBytes (LedgerBytes), toBuiltin)
import PlutusTx (compile, CompiledCode)
import Codec.Serialise (serialise)
import Cardano.Api (writeFileTextEnvelope, PlutusScriptV1, SerialiseAsCBOR (deserialiseFromCBOR, serialiseToCBOR), AsType (AsScript, AsPlutusScriptV1), Script)
import qualified Data.ByteString.Lazy as B
import Cardano.Binary (DecoderError)
import qualified Data.ByteString.Short as SBS
import Flat
import Data.ByteString (ByteString)
import Prelude
import Data.Either

-- import PlutusV1LedgerApi

alwaysSucceeds :: BuiltinData -> BuiltinData -> ()
alwaysSucceeds _ _ = ()

-- alwaysSucceedsCompiled :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
alwaysSucceedsCompiled :: CompiledCode (BuiltinData -> BuiltinData -> ())
alwaysSucceedsCompiled = $$(PlutusTx.compile [|| alwaysSucceeds ||])

toCardanoApiScript :: CompiledCode a -> ByteString
toCardanoApiScript = B.toStrict . serialise . fromCompiledCode

-- toCardanoApiScript :: CompiledCode a -> Either DecoderError (Script PlutusScriptV1)
-- toCardanoApiScript =  serialiseCompiledCode 

f = serialiseCompiledCode 

main = do
  let x = toCardanoApiScript alwaysSucceedsCompiled
  print $ LedgerBytes $ toBuiltin x 
  case deserialiseFromCBOR (AsScript AsPlutusScriptV1) $ x of
    Left err -> print err
    Right script ->
      -- writeFileTextEnvelope @(Script PlutusScriptV1) "always-succeeds.plutus" (Just "My little script") script
      writeFileTextEnvelope @(Script PlutusScriptV1) "always-succeeds.plutus" Nothing script
      >>= either print return


module Main where

-- import Ledger (fromCompiledCode, Redeemer, Datum, ScriptContext)
-- import Plutus.V1.Ledger.Api (BuiltinData, LedgerBytes (LedgerBytes), toBuiltin)
import PlutusTx (compile, CompiledCode, BuiltinData)
import Codec.Serialise (serialise)
-- import Cardano.Api (writeFileTextEnvelope, PlutusScriptV1, SerialiseAsCBOR (deserialiseFromCBOR, serialiseToCBOR), AsType (AsScript, AsPlutusScriptV1), Script)
import qualified Data.ByteString.Lazy as B
-- import Cardano.Binary (DecoderError)
import qualified Data.ByteString.Short as SBS
-- import Flat
import Data.ByteString (ByteString)

import Prelude
import Data.Either

alwaysSucceeds :: BuiltinData -> BuiltinData -> ()
alwaysSucceeds _ _ = ()

-- alwaysSucceedsCompiled :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
alwaysSucceedsCompiled = $$(PlutusTx.compile [|| alwaysSucceeds ||])

toCardanoApiScript :: CompiledCode a -> ByteString
toCardanoApiScript = B.toStrict . serialise . fromCompiledCode

-- toCardanoApiScript :: CompiledCode a -> Either DecoderError (Script PlutusScriptV1)
-- toCardanoApiScript =  serialiseCompiledCode 

-- f = PlutusScript  . SBS.toShort . B.toStrict

main = do
  let x = toCardanoApiScript alwaysSucceedsCompiled
  print $ LedgerBytes $ toBuiltin x 
  -- case deserialiseFromCBOR (AsScript AsPlutusScriptV1) $ x of
  --   Left err -> print err
  --   Right script ->
  --     -- writeFileTextEnvelope @(Script PlutusScriptV1) "always-succeeds.plutus" (Just "My little script") script
  --     writeFileTextEnvelope @(Script PlutusScriptV1) "always-succeeds.plutus" Nothing script
  --     >>= either print return


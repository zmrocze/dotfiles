-- {-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:dump-uplc #-}
module Main where


import Ledger (fromCompiledCode, Redeemer, Datum, ScriptContext)
import Plutus.V1.Ledger.Api (BuiltinData, LedgerBytes (LedgerBytes), toBuiltin)
import PlutusTx (compile, CompiledCode)
import Codec.Serialise (serialise)

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Short as SBS
import Data.ByteString (ByteString)
import Data.Either

-- import PlutusLedgerApi

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

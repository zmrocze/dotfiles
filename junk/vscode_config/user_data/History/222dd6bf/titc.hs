{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module AlwaysSucceeds where

import Plutus.V2.Ledger.Contexts (ScriptContext)
import PlutusTx.Prelude (Bool (True), ($))
import Ledger.Typed.Scripts (ValidatorTypes(..), TypedValidator, Validator)
import qualified PlutusTx
import Ledger (Address, fromCompiledCode, Validator (Validator), validatorHash, scriptHashAddress)
import qualified Plutus.Script.Utils.V2.Typed.Scripts as Scripts


{-# INLINEABLE mkAlwaysSucceeds #-}
mkAlwaysSucceeds :: () -> () -> ScriptContext -> Bool
mkAlwaysSucceeds _ _ _ = True

data AlwaysFail

instance ValidatorTypes AlwaysFail where
  type DatumType AlwaysFail = ()
  type RedeemerType AlwaysFail = ()

typedValidator :: TypedValidator AlwaysFail
typedValidator =
  Scripts.mkTypedValidator @AlwaysFail
    $$(PlutusTx.compile [||mkAlwaysSucceeds||])
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.mkUntypedValidator @() @()

compiled :: Validator
compiled = Validator $ fromCompiledCode $$(PlutusTx.compile [||mkAlwaysSucceeds||])

-- validator :: Versioned Validator
-- validator = Versioned compiled PlutusV1

validatorAddr :: Address
validatorAddr = scriptHashAddress (validatorHash compiled)
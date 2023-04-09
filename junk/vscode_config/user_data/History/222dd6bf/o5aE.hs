{-# LANGUAGE TemplateHaskell #-}

module AlwaysSucceeds where

import Plutus.V1.Ledger.Contexts (ScriptContext)



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
    $$(PlutusTx.compile [||mkValidator||])
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.mkUntypedValidator @() @()

compiled :: Validator
compiled = Validator $ fromCompiledCode $$(PlutusTx.compile [||mkValidator||])

validator :: Versioned Validator
validator = Versioned compiled PlutusV1

validatorAddr :: Address
validatorAddr = scriptHashAddress (validatorHash validator)
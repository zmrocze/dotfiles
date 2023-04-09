{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-specialise  #-}
module AlwaysSucceeds where

import Plutus.V2.Ledger.Contexts (ScriptContext)
import PlutusTx.Prelude (Bool (True), ($))
import Ledger.Typed.Scripts (ValidatorTypes(..), Validator)
import qualified PlutusTx
import Ledger (Address, fromCompiledCode, Validator (Validator), validatorHash, scriptHashAddress)
import qualified Plutus.Script.Utils.V2.Typed.Scripts as Scripts
import Plutus.Script.Utils.V2.Typed.Scripts.Validators (TypedValidator)


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
    {-# INLINEABLE wrap #-}
    wrap = Scripts.mkUntypedValidator @() @()

compiled :: Validator
compiled = Validator $ fromCompiledCode $$(PlutusTx.compile [||mkAlwaysSucceeds||])

-- validator :: Versioned Validator
-- validator = Versioned compiled PlutusV1

validatorAddr :: Address
validatorAddr = scriptHashAddress (validatorHash compiled)
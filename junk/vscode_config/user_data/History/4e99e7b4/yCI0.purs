module Ctl.Internal.ApplyArgs
  ( applyArgs,
    applyArgsWithErrors
  )
  where

import Contract.Prelude (Either(..), Maybe(..), ($))
import Ctl.Internal.Deserialization.WitnessSet as D
import Ctl.Internal.Serialization.PlutusData (convertPlutusData) as S
import Ctl.Internal.Serialization.PlutusScript (convertPlutusScript) as S
import Ctl.Internal.Serialization.Types as CSL
import Ctl.Internal.Types.PlutusData (PlutusData(List))
import Ctl.Internal.Types.Scripts (PlutusScript)

foreign import apply_params_to_script :: CSL.PlutusData -> CSL.PlutusScript -> CSL.PlutusScript
-- foreign import apply_params_to_script_with_errors ::  CSL.PlutusData -> CSL.PlutusScript -> CSL.PlutusScript
foreign import apply_params_to_script_with_errors 
  :: (forall x. x -> Either x CSL.PlutusScript) -> (forall x. x -> Either String x)
  -> CSL.PlutusData -> CSL.PlutusScript -> Either String CSL.PlutusScript

apply_params_to_script_either :: CSL.PlutusData -> CSL.PlutusScript -> Either String CSL.PlutusScript
apply_params_to_script_either = apply_params_to_script_with_errors Left Right

applyArgs :: PlutusScript -> Array PlutusData -> Maybe PlutusScript
applyArgs script args =
  case (S.convertPlutusData (List args)) of
    Nothing -> Nothing
    Just args1 -> D.convertPlutusScript $ apply_params_to_script args1 (S.convertPlutusScript script)


applyArgsWithErrors  :: PlutusScript -> Array PlutusData -> Maybe PlutusScript
applyArgsWithErrors script args =
  case (S.convertPlutusData (List args)) of
    Nothing -> Nothing
    Just args1 -> D.convertPlutusScript $ apply_params_to_script_either args1 (S.convertPlutusScript script)
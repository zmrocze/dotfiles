module Ctl.Internal.ApplyArgs
  ( ApplyArgsError(..)
  , applyArgs
  , applyArgsWithErrors
  )
  where

import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)

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

newtype ApplyArgsError = ApplyArgsError String

derive instance Newtype Epoch _
derive instance Generic Epoch _
derive instance EncodeAeson Epoch

instance Show Epoch where
  show = genericShow

applyArgsWithErrors  :: PlutusScript -> Array PlutusData -> Either ApplyArgsError PlutusScript
applyArgsWithErrors script paramsList = left ApplyArgsError do
  params <- note "Error converting to serialized PlutusData" $ S.convertPlutusData (List paramsList)
  appliedScript <- apply_params_to_script_either params (S.convertPlutusScript script)
  note "Error converting back applied script" $ D.convertPlutusScript $ appliedScript
module Ctl.Internal.ApplyArgs
  ( ApplyArgsError(..)
  , applyArgs
  , applyArgsWithErrors
  )
  where

import Contract.Prelude (Either(..), Maybe(..), bind, note, ($))
import Ctl.Internal.Deserialization.WitnessSet as D
import Ctl.Internal.Serialization.PlutusData (convertPlutusData) as S
import Ctl.Internal.Serialization.PlutusScript (convertPlutusScript) as S
import Ctl.Internal.Serialization.Types as CSL
import Ctl.Internal.Types.PlutusData (PlutusData(List))
import Ctl.Internal.Types.Scripts (PlutusScript)
import Data.Profunctor.Choice (left)

import Ctl.Internal.Serialization.ToBytes (toBytes)
import Ctl.Internal.Serialization.Types (Ed25519Signature, PublicKey) as Serialization
import Ctl.Internal.ToData (class ToData, toData)
import Ctl.Internal.Types.Aliases (Bech32String)
import Ctl.Internal.Types.BigNum (BigNum)
import Ctl.Internal.Types.ByteArray (ByteArray)
import Ctl.Internal.Types.Int as Int
import Ctl.Internal.Types.OutputDatum (OutputDatum)
import Ctl.Internal.Types.PlutusData (PlutusData)
import Ctl.Internal.Types.PubKeyHash (PaymentPubKeyHash)
import Ctl.Internal.Types.RawBytes (RawBytes)
import Ctl.Internal.Types.RedeemerTag (RedeemerTag)
import Ctl.Internal.Types.RewardAddress (RewardAddress)
import Ctl.Internal.Types.Scripts (Language, PlutusScript)
import Ctl.Internal.Types.Transaction (TransactionInput)
import Ctl.Internal.Types.TransactionMetadata (GeneralTransactionMetadata)
import Ctl.Internal.Types.VRFKeyHash (VRFKeyHash)
import Data.Array (union)
import Data.BigInt (BigInt)
import Data.Either (Either(Left), note)
import Data.Generic.Rep (class Generic)
import Data.Lens (lens')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Lens.Types (Lens')
import Data.Map (Map)
import Data.Maybe (Maybe(Nothing), fromJust)
import Data.Monoid (guard)
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
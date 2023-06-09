-- File auto generated by purescript-bridge! --
module Ledger.Index where

import Prelude

import Control.Lazy (defer)
import Data.Argonaut (encodeJson, jsonNull)
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Decode.Aeson ((</$\>), (</*\>), (</\>))
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Encode.Aeson ((>$<), (>/\<))
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Lens (Iso', Lens', Prism', iso, prism')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.RawJson (RawJson)
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Plutus.V1.Ledger.Scripts (MintingPolicy, ScriptError, Validator)
import Type.Proxy (Proxy(Proxy))
import Data.Argonaut.Decode.Aeson as D
import Data.Argonaut.Encode.Aeson as E
import Data.Map as Map

data ScriptType
  = ValidatorScript Validator String
  | MintingPolicyScript MintingPolicy

derive instance Eq ScriptType

instance Show ScriptType where
  show a = genericShow a

instance EncodeJson ScriptType where
  encodeJson = defer \_ -> case _ of
    ValidatorScript a b -> E.encodeTagged "ValidatorScript" (a /\ b) (E.tuple (E.value >/\< E.value))
    MintingPolicyScript a -> E.encodeTagged "MintingPolicyScript" a E.value

instance DecodeJson ScriptType where
  decodeJson = defer \_ -> D.decode
    $ D.sumType "ScriptType"
    $ Map.fromFoldable
        [ "ValidatorScript" /\ D.content (D.tuple $ ValidatorScript </$\> D.value </*\> D.value)
        , "MintingPolicyScript" /\ D.content (MintingPolicyScript <$> D.value)
        ]

derive instance Generic ScriptType _

--------------------------------------------------------------------------------

_ValidatorScript :: Prism' ScriptType { a :: Validator, b :: String }
_ValidatorScript = prism' (\{ a, b } -> (ValidatorScript a b)) case _ of
  (ValidatorScript a b) -> Just { a, b }
  _ -> Nothing

_MintingPolicyScript :: Prism' ScriptType MintingPolicy
_MintingPolicyScript = prism' MintingPolicyScript case _ of
  (MintingPolicyScript a) -> Just a
  _ -> Nothing

--------------------------------------------------------------------------------

data ScriptValidationEvent
  = ScriptValidationEvent
      { sveScript :: String
      , sveResult :: Either ScriptError (Tuple RawJson (Array String))
      , sveRedeemer :: String
      , sveType :: ScriptType
      }
  | ScriptValidationResultOnlyEvent { sveResult :: Either ScriptError (Tuple RawJson (Array String)) }

derive instance Eq ScriptValidationEvent

instance Show ScriptValidationEvent where
  show a = genericShow a

instance EncodeJson ScriptValidationEvent where
  encodeJson = defer \_ -> case _ of
    ScriptValidationEvent { sveScript, sveResult, sveRedeemer, sveType } -> encodeJson
      { tag: "ScriptValidationEvent"
      , sveScript: flip E.encode sveScript E.value
      , sveResult: flip E.encode sveResult (E.either E.value (E.tuple (E.value >/\< E.value)))
      , sveRedeemer: flip E.encode sveRedeemer E.value
      , sveType: flip E.encode sveType E.value
      }
    ScriptValidationResultOnlyEvent { sveResult } -> encodeJson
      { tag: "ScriptValidationResultOnlyEvent"
      , sveResult: flip E.encode sveResult (E.either E.value (E.tuple (E.value >/\< E.value)))
      }

instance DecodeJson ScriptValidationEvent where
  decodeJson = defer \_ -> D.decode
    $ D.sumType "ScriptValidationEvent"
    $ Map.fromFoldable
        [ "ScriptValidationEvent" /\
            ( ScriptValidationEvent <$> D.object "ScriptValidationEvent"
                { sveScript: D.value :: _ String
                , sveResult: (D.either D.value (D.tuple (D.value </\> D.value))) :: _ (Either ScriptError (Tuple RawJson (Array String)))
                , sveRedeemer: D.value :: _ String
                , sveType: D.value :: _ ScriptType
                }
            )
        , "ScriptValidationResultOnlyEvent" /\ (ScriptValidationResultOnlyEvent <$> D.object "ScriptValidationResultOnlyEvent" { sveResult: (D.either D.value (D.tuple (D.value </\> D.value))) :: _ (Either ScriptError (Tuple RawJson (Array String))) })
        ]

derive instance Generic ScriptValidationEvent _

--------------------------------------------------------------------------------

_ScriptValidationEvent :: Prism' ScriptValidationEvent { sveScript :: String, sveResult :: Either ScriptError (Tuple RawJson (Array String)), sveRedeemer :: String, sveType :: ScriptType }
_ScriptValidationEvent = prism' ScriptValidationEvent case _ of
  (ScriptValidationEvent a) -> Just a
  _ -> Nothing

_ScriptValidationResultOnlyEvent :: Prism' ScriptValidationEvent { sveResult :: Either ScriptError (Tuple RawJson (Array String)) }
_ScriptValidationResultOnlyEvent = prism' ScriptValidationResultOnlyEvent case _ of
  (ScriptValidationResultOnlyEvent a) -> Just a
  _ -> Nothing

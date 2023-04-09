module Test.Ctl.ApplyArgs (main, suite) where

import Contract.Prelude

import Contract.Monad (launchAff_)
import Contract.PlutusData (PlutusData(..), toData)
import Contract.Prim.ByteArray (hexToByteArrayUnsafe)
import Contract.TextEnvelope (decodeTextEnvelope, plutusScriptV1FromEnvelope, plutusScriptV2FromEnvelope)
import Control.Monad.Error.Class (class MonadError)
import Ctl.Examples.IncludeDatum (only42Script)
import Ctl.Examples.PlutusV2.Scripts.AlwaysSucceeds (alwaysSucceedsScriptV2)
import Ctl.Internal.ApplyArgs (applyArgs)
import Ctl.Internal.Cardano.TextEnvelope (TextEnvelope(..))
import Ctl.Internal.Cardano.Types.Value (scriptHashAsCurrencySymbol)
import Ctl.Internal.Helpers ((<</>>))
import Ctl.Internal.Plutip.Server (stopChildProcessWithPort)
import Ctl.Internal.Plutip.Spawn (ManagedProcess, NewOutputAction(NoOp, Success), spawn)
import Ctl.Internal.QueryM (ClientError(ClientEncodingError), handleAffjaxResponse, mkHttpUrl, postAeson, scriptToAeson)
import Ctl.Internal.Serialization (toBytes) as Serialization
import Ctl.Internal.Serialization.PlutusData (convertPlutusData) as Serialization
import Ctl.Internal.Test.TestPlanM (TestPlanM, interpret)
import Data.Array.NonEmpty (toUnfoldable)
import Data.BigInt (fromInt)
import Data.List.Lazy (replicate)
import Data.Map (Map)
import Data.Profunctor.Choice (left)
import Data.String (Pattern(Pattern))
import Data.String as String
import Data.UInt (UInt)
import Data.UInt as UInt
import Effect.Aff (Error, bracket, error, runAff_, throwError)
import Foreign.Object (Object)
import Foreign.Object as Object
import Mote (group, test)
import Node.ChildProcess (defaultSpawnOptions)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Node.Globals (__dirname)
import Untagged.Union (asOneOf)

foreign import scripts :: Object String

main ∷ Effect Unit
main = launchAff_ $ interpret $ suite

suite ∷ TestPlanM (Aff Unit) Unit
suite = group "Applying params to scripts test" $ do
  traverse_ (uncurry $ testCase v1) $ Tuple <$> v1ScriptPaths <*> params
  traverse_ (uncurry $ testCase v2) $ Tuple <$> v2ScriptPaths <*> params

  where

    testCase lang scriptName (args /\ argsName) = test ("Apply " <> argsName <> " to " <> scriptName) do 
      script <- lang scriptName
      applied <- liftEither $ left (error <<< show) $ applyArgs script args
      appliedShouldBe <- lang (scriptName <> "-" <> argsName)
      if applied == appliedShouldBe then do
        pure unit
      else
        throwError (error $ "Result of applying params to a script should be: " <> show appliedShouldBe <> " but is instead: " <> show applied)

    v1ScriptPaths =
      [ "always-fails"
      , "include-datum"
      , "one-shot-minting"
      , "redeemer1-validator"]

    v2ScriptPaths =
      ["always-succeeds-v2"
      , "one-shot-minting-v2"
      , "check-datum-is-inline"
      ]

    params = [
      ([] /\ "no-args")
      , ([ un ] /\ "unit")
      , ([ i 7, un, List [ un, bytes ], longBytes, Map [ (i 5 /\ i 7), (bytes /\ i 8) ],
          Constr (fromInt 102) [i 7, List [ un, bytes, longBytes ]],
          Constr (fromInt 5) [List [], List [i 1], Map [], Map [(i 1 /\ un), (i 2 /\ Constr (fromInt 2) [i 2])]]
         ] /\ "big-arg")
      ]
    i k = toData (fromInt k)
    un = toData unit
    bytes = Bytes $ hexToByteArrayUnsafe "4d5f"
    longBytes = Bytes $ hexToByteArrayUnsafe $ foldl (\x y -> x <> y) "" $
      replicate 65 "4d"

    lookupAux :: forall m. MonadError Error m => (TextEnvelope -> Maybe PlutusScript) -> String -> m PlutusScript
    lookupAux decodeScript name = maybe (throwError $ error $ "Can't find the script with name " <> name) pure $ do 
      txt <- Object.lookup name scripts
      envelope <- decodeTextEnvelope txt
      decodeScript envelope

    v1 :: forall m . MonadError Error m => String -> m PlutusScript
    v1 = lookupAux plutusScriptV1FromEnvelope

    v2 :: forall m . MonadError Error m => String -> m PlutusScript
    v2 =  lookupAux plutusScriptV2FromEnvelope


module GenScriptsFFI where
import Contract.Prelude (bind)

decodePlutusScript = do
  env <- decodeTextEnvelope
  case env.type_ of 
    PlutusScriptV1 -> plutusV1Script env.bytes
    PlutusScriptV2 -> plutusV2Script env.bytes
    PaymentSigningKeyShelleyed25519 -> Nothing
    StakeSigningKeyShelleyed25519 -> Nothing
    Other other -> Nothing


plutusScriptFromEnvelope
  :: TextEnvelopeType
  -> (ByteArray -> PlutusScript)
  -> TextEnvelope
  -> Maybe PlutusScript
plutusScriptFromEnvelope type_ bytesToScript (TextEnvelope envelope) = do
  -- Check TextEnvelope type match to desirable
  unless (envelope.type_ == type_) Nothing
  pure $ bytesToScript envelope.bytes
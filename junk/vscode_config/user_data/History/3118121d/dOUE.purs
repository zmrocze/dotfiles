
module GenScriptsFFI where

decodePlutusScript = do
  env <- decodeTextEnvelope
  case env._type of 
    PlutusScriptV1 -> plutusV1Script 


plutusScriptFromEnvelope
  :: TextEnvelopeType
  -> (ByteArray -> PlutusScript)
  -> TextEnvelope
  -> Maybe PlutusScript
plutusScriptFromEnvelope type_ bytesToScript (TextEnvelope envelope) = do
  -- Check TextEnvelope type match to desirable
  unless (envelope.type_ == type_) Nothing
  pure $ bytesToScript envelope.bytes
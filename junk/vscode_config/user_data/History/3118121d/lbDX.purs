
module GenScriptsFFI where

decodePlutusScript = do
  env <- decodeTextEnvelope
  case env._type of 
    PlutusScript
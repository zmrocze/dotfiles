
module Cardano.Api.Modes.Orphans where

import Data.Aeson (FromJSON(parseJSON), Value, Parser)
import Cardano.Api (EraInMode(BabbageEraInCardanoMode), BabbageEra, CardanoMode)


instance FromJSON (EraInMode BabbageEra CardanoMode) where
  parseJSON "BabbageEraInCardanoMode" = pure BabbageEraInCardanoMode
  parseJSON invalid =
      invalidJSONFailure "BabbageEraInCardanoMode"
                         "parsing 'EraInMode Babbage CardanoMode' failed, "
                         invalid

invalidJSONFailure :: String -> String -> Value -> Parser a
invalidJSONFailure expectedType errorMsg invalidValue =
    prependFailure errorMsg
                   (typeMismatch expectedType invalidValue)
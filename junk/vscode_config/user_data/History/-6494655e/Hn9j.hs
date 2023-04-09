
module Cardano.Api.Modes.Orphans where

instance FromJSON (EraInMode BabbageEra CardanoMode) where
  parseJSON "BabbageEraInCardanoMode" = pure BabbageEraInCardanoMode
  parseJSON invalid =
      invalidJSONFailure "BabbageEraInCardanoMode"
                         "parsing 'EraInMode Babbage CardanoMode' failed, "
                         invalid


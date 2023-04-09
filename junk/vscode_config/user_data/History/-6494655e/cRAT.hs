
module Cardano.Api.Modes.Orphans where

import Data.Aeson (FromJSON(parseJSON), invalidJSONFailure)
import Cardano.Api (EraInMode, BabbageEraInCardanoMode)


instance FromJSON (EraInMode BabbageEra CardanoMode) where
  parseJSON "BabbageEraInCardanoMode" = pure BabbageEraInCardanoMode
  parseJSON invalid =
      invalidJSONFailure "BabbageEraInCardanoMode"
                         "parsing 'EraInMode Babbage CardanoMode' failed, "
                         invalid


{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia        #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Ledger.DCert.Orphans where

import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)

import Ledger.Crypto.Orphans ()
import Plutus.V1.Ledger.DCert

deriving anyclass instance ToJSON DCert
deriving anyclass instance FromJSON DCert


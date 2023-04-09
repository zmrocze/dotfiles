{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia        #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Ledger.DCert.Orphans where

import Data.Aeson (FromJSON, ToJSON)

import Ledger.Crypto.Orphans ()
import Ledger.Credential.Orphans ()
import Plutus.V1.Ledger.DCert ( DCert )

deriving anyclass instance ToJSON DCert
deriving anyclass instance FromJSON DCert


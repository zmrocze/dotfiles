{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia        #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Ledger.DCert.Orphans where

deriving anyclass instance ToJSON DCert
deriving anyclass instance FromJSON DCert


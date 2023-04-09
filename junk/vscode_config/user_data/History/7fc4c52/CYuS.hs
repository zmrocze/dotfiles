{-# OPTIONS_GHC -Wno-orphans #-}

module Ledger.Scripts.Orphans where

import Plutus.V1.Ledger.Scripts (MintingPolicyHash)
import Control.DeepSeq (NFData)

instance NFData MintingPolicyHash
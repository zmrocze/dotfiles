
{-# LANGUAGE StandaloneDeriving #-}

module Ledger.Contexts.Orphans where 
import Plutus.V1.Ledger.Contexts (ScriptPurpose(..))

deriving instance Ord ScriptPurpose
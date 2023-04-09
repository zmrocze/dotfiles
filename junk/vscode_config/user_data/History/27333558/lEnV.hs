module Ledger.Contexts
    ( module Export
    , pubKeyHash
    , plutusV1ScriptCurrencySymbol
    , plutusV2ScriptCurrencySymbol
    ) where

import Ledger.Crypto (pubKeyHash)
import Ledger.Scripts (MintingPolicy, MintingPolicyHash (..))
import Plutus.Script.Utils.V1.Scripts qualified as PV1
import Plutus.Script.Utils.V2.Scripts qualified as PV2
import Plutus.V1.Ledger.Contexts as Export
import Plutus.V1.Ledger.Value (CurrencySymbol (..))
import Plutus.V1.Ledger.Value qualified as Value

{-# INLINABLE plutusV1ScriptCurrencySymbol #-}
-- | The 'CurrencySymbol' of a 'MintingPolicy'
plutusV1ScriptCurrencySymbol :: MintingPolicy -> CurrencySymbol
plutusV1ScriptCurrencySymbol scrpt =
    let (MintingPolicyHash hsh) = PV1.mintingPolicyHash scrpt in Value.CurrencySymbol hsh

{-# INLINABLE plutusV2ScriptCurrencySymbol #-}
-- | The 'CurrencySymbol' of a 'MintingPolicy'
plutusV2ScriptCurrencySymbol :: MintingPolicy -> CurrencySymbol
plutusV2ScriptCurrencySymbol scrpt =
    let (MintingPolicyHash hsh) = PV2.mintingPolicyHash scrpt in Value.CurrencySymbol hsh


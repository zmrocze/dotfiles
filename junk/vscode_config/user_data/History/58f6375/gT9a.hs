{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module Indigo.Contracts.Vesting.OnChain
  ( validateVesting,
    compiledValidateVestingScript,
    untypedVestingValidatorHash,
    vestingScriptCTL,
  )
where

import Indigo.Contracts.Governance.VersionRegistry.Common
  ( validateUpgradeVersion,
  )
import Indigo.Contracts.Vesting.Common
  ( VestingRedeemer (Unlock, UpgradeVersion),
    VestingScriptParams (MkVestingScriptParams),
    calculateTotalVested,
    distributorAddress,
    indyToken,
    versionRecordToken,
    vestingIndyTotalAmt,
  )
import Indigo.Utils.Helpers qualified as Helpers
import Indigo.Utils.Utils qualified as Utils
import Ledger.Value (assetClassValue, assetClassValueOf)
import Plutus.Script.Utils.V2.Scripts qualified as Scripts
import Plutus.Script.Utils.V2.Typed.Scripts.Validators
  ( UntypedValidator,
    mkUntypedValidator,
  )
import Plutus.V2.Ledger.Api qualified as V2
import Plutus.V2.Ledger.Contexts qualified as Contexts
import PlutusTx qualified
import PlutusTx.Prelude

{- The purpose of this script is to hold the INDY rewards to be distributed
   periodically to some of the actors partipicating in the Indigo protocol
   (namely stability pool stakers, voters and DEX liquidity providers).

   The locked funds can be unlocked in either of the following two situations:

     - Funds have been vested and not unlocked yet. In this case the unlocked
       funds must go to a designated address responsible for their distribution.

     - The rewards are being migrated to an upgraded version of the script.
-}
{-# INLINEABLE validateVesting #-}
validateVesting ::
  VestingScriptParams -> () -> VestingRedeemer -> V2.ScriptContext -> Bool
-- Vested INDY is being unlocked.
validateVesting params _ Unlock ctx = validateUnlock params ctx
-- INDY rewards are being migrated to an upgraded version of this script.
validateVesting MkVestingScriptParams {versionRecordToken} _ UpgradeVersion ctx =
  validateUpgradeVersion "Vesting" ctx versionRecordToken

{- Verifies that only vested INDY that has not been previously unlocked
   is being unlocked and that it is sent to the correct address.
-}
{-# INLINEABLE validateUnlock #-}
validateUnlock :: VestingScriptParams -> V2.ScriptContext -> Bool
validateUnlock MkVestingScriptParams {distributorAddress, indyToken} ctx =
  {-
     This is to prevent a double satisfaction attack, where INDY coming
     from a different UTxO (from this script or a different one) is used
     to validate the transaction.

     It also ensures that there is a single UTxO containing INDY in the
     inputs of the transaction.
  -}
  traceIfFalse
    "Only one UTxO holding INDY can be consumed"
    (indySpent == ownInputIndy)
    {-
       UTxOs locked by this script are meant to be consumed
       only when unlocking vested INDY.
    -}
    && traceIfFalse "Nothing to unlock" (indyAmtToUnlock > 0)
    {-
       The INDY not vested yet must remain locked in the script,
       in a single UTxO and with no staking credential attached.
    -}
    && traceIfFalse
      "Incorrect vesting script output"
      ( Helpers.checkOwnOutputNoStakingAdaGeq
          ctx
          ()
          (assetClassValue indyToken lockedIndy)
          || lockedIndy == 0
      )
    {-
       The difference in INDY between inputs and outputs coming from
       and going to this script must be sent to the distributor's address.
    -}
    && traceIfFalse
      "Incorrect value sent to distributor's address"
      ( Helpers.checkOutputToAddressAdaGeq
          info
          distributorAddress
          (assetClassValue indyToken indyAmtToUnlock)
      )
  where
    info :: V2.TxInfo
    info = V2.scriptContextTxInfo ctx

    -- The total amount of INDY in the inputs of the transaction.
    indySpent :: Integer
    indySpent = assetClassValueOf (Contexts.valueSpent info) indyToken

    {-
       The amount of INDY in the input being validated.

       Note: there could be more than one UTxO holding INDY locked by this
       script if somebody locked an extra UTxO in this script. However:
        - The validator conditions ensure that only UTxOs holding enough INDY
          to lock back the unvested amount can be used, so cheap mess-ups are
          avoided.
        - Only one such UTxO can be used per transaction, so duplicates
          received by the distributor's address are trivial to identify.
    -}
    ownInputIndy :: Integer
    ownInputIndy =
      assetClassValueOf
        (V2.txOutValue $ V2.txInInfoResolved $ Helpers.findOwnInput' ctx)
        indyToken

    {- The amount of INDY vested until the current time.

       The current time is approximated by the lower bound of the transaction
       validity range, as it is in the best interest of the actor unlocking
       INDY to set it as close to the current time as possible without
       exceeding it (to be able to unlock the maximum amount of INDY).
    -}
    vestedIndy :: Integer
    vestedIndy =
      calculateTotalVested
        ( fromMaybe
            (traceError "Validity range doesn't have finite lower bound")
            (Helpers.validityRangeFiniteLowerBound info)
        )

    -- The amount of INDY that must remain locked by this script.
    lockedIndy :: Integer
    lockedIndy = vestingIndyTotalAmt - vestedIndy

    -- The amount of INDY to be unlocked and sent somewhere else.
    indyAmtToUnlock :: Integer
    indyAmtToUnlock = Utils.zeroNegatives (ownInputIndy - lockedIndy)

-----------------

compiledValidateVestingScript ::
  PlutusTx.CompiledCode (VestingScriptParams -> UntypedValidator)
compiledValidateVestingScript =
  Helpers.optimizeUPLC
    $$(PlutusTx.compile [||mkUntypedValidator . validateVesting||])

compiledUntypedValidateVestingScript ::
  PlutusTx.CompiledCode (BuiltinData -> UntypedValidator)
compiledUntypedValidateVestingScript =
  Helpers.optimizeUPLC
    $$( PlutusTx.compile
          [||
          mkUntypedValidator
            . validateVesting
            . PlutusTx.unsafeFromBuiltinData
          ||]
      )

untypedVestingValidator :: BuiltinData -> V2.Validator
untypedVestingValidator params =
  V2.mkValidatorScript
    ( compiledUntypedValidateVestingScript
        `PlutusTx.applyCode` PlutusTx.liftCode params
    )

untypedVestingValidatorHash :: BuiltinData -> V2.ValidatorHash
untypedVestingValidatorHash = Scripts.validatorHash . untypedVestingValidator

-- serialised for use in CTL
vestingScriptCTL :: V2.Script
vestingScriptCTL = V2.fromCompiledCode compiledUntypedValidateVestingScript

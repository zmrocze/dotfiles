{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Indigo.Contracts.Vesting.Common
  ( VestingScriptParams
      ( MkVestingScriptParams,
        distributorAddress,
        indyToken,
        versionRecordToken
      ),
    VestingRedeemer (Unlock, UpgradeVersion),
    VestingScript,
    vestingIndyTotalAmt,
    spDistributionSchedule,
    govDistributionSchedule,
    liqDistributionSchedule,
    calculateVestedPerSchedule,
    calculateTotalVested,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Indigo.Data.Decimal (decimalUnit)
import Indigo.Utils.Helpers (oneDay)
import Indigo.Utils.Utils qualified as Utils
import Ledger (Address, POSIXTime (POSIXTime, getPOSIXTime))
import Ledger.Typed.Scripts qualified as TScripts
import Ledger.Value (AssetClass)
import PlutusTx qualified
import PlutusTx.Builtins (divideInteger)
import PlutusTx.Prelude
import Prelude qualified as P

--------------
-- Data types
--------------

type UnlockAmount = Integer

type MaxUnlockable = Integer

-- | `MaxUnlockable` is the maximum amount that can be vested for the schedule.
-- The vested amount is therefore clamped to this maximum.
--
-- The actual schedule is represented by a list of tuples where:
--   - The first item in the tuple represents the date in POSIXTime from when
--     the vesting amount in the second item applies.
--   - The second item in the tuple represents the amount of INDY vested per
--     epoch in the period starting from the date in the first item (and up to,
--     but not including, the next date in the list).

-- Assumptions:
--   - The list is assumed to be sorted in ascending order of dates.
--   - The UnlockAmount is assumed to be a natural number.
type VestingSchedule = (MaxUnlockable, [(POSIXTime, UnlockAmount)])

data VestingScriptParams = MkVestingScriptParams
  { -- | Address receiving unlocked INDY.
    distributorAddress :: Address,
    -- | The asset class of the INDY token.
    indyToken :: AssetClass,
    -- | Token for identifying the version record for a protocol upgrade.
    versionRecordToken :: AssetClass
  }
  deriving (Generic, P.Show, ToJSON, FromJSON)

PlutusTx.makeLift ''VestingScriptParams
PlutusTx.makeIsDataIndexed ''VestingScriptParams [('MkVestingScriptParams, 0)]

data VestingRedeemer
  = -- | Unlock to take INDY that should be unlocked based
    -- on the distribution schedules.
    Unlock
  | -- | UpgradeVersion to upgrade the script.
    UpgradeVersion

PlutusTx.makeLift ''VestingRedeemer
PlutusTx.makeIsDataIndexed
  ''VestingRedeemer
  [ ('Unlock, 0),
    ('UpgradeVersion, 1)
  ]

data VestingScript

instance TScripts.ValidatorTypes VestingScript where
  type DatumType VestingScript = ()
  type RedeemerType VestingScript = VestingRedeemer

-----------
-- Values
-----------

-- Total amount of INDY to be distributed as rewards.
vestingIndyTotalAmt :: Integer
vestingIndyTotalAmt = 21_000_000 * decimalUnit

-- Vesting schedule for Stability Pool rewards.
spDistributionSchedule :: VestingSchedule
spDistributionSchedule =
  ( 14_000_000 * decimalUnit,
    [ -- 2022-12-01, 21:45 UTC
      (Ledger.POSIXTime 1669931100000, 28_768 * decimalUnit),
      -- 2023-12-01, 21:45 UTC
      (Ledger.POSIXTime 1701467100000, 33_562 * decimalUnit),
      -- 2024-09-26, 21:45 UTC
      (Ledger.POSIXTime 1727387100000, 33_561 * decimalUnit),
      -- 2024-11-30, 21:45 UTC
      (Ledger.POSIXTime 1733003100000, 38_356 * decimalUnit),
      -- 2025-11-30, 21:45 UTC
      (Ledger.POSIXTime 1764539100000, 43_150 * decimalUnit),
      -- 2026-11-30, 21:45 UTC
      (Ledger.POSIXTime 1796075100000, 47_945 * decimalUnit)
    ]
  )

-- Vesting schedule for DEX liquidity rewards.
liqDistributionSchedule :: VestingSchedule
liqDistributionSchedule =
  ( 5_250_000 * decimalUnit,
    [ -- 2022-12-21, 21:45 UTC
      (Ledger.POSIXTime 1671659100000, 4_795 * decimalUnit),
      -- 2023-12-21, 21:45 UTC
      (Ledger.POSIXTime 1703195100000, 9_590 * decimalUnit),
      -- 2024-10-11, 21:45 UTC
      (Ledger.POSIXTime 1728683100000, 9_589 * decimalUnit),
      -- 2024-12-20, 21:45 UTC
      (Ledger.POSIXTime 1734731100000, 14_383 * decimalUnit),
      -- 2025-12-20, 21:45 UTC
      (Ledger.POSIXTime 1766267100000, 19_178 * decimalUnit),
      -- 2026-12-20, 21:45 UTC
      (Ledger.POSIXTime 1797803100000, 23_972 * decimalUnit)
    ]
  )

-- Vesting schedule for Governance rewards.
govDistributionSchedule :: VestingSchedule
govDistributionSchedule =
  ( 1_750_000 * decimalUnit,
    [ -- 2022-12-06, 21:45 UTC
      (Ledger.POSIXTime 1670363100000, 2_398 * decimalUnit),
      -- 2023-12-06, 21:45 UTC
      (Ledger.POSIXTime 1701899100000, 3_596 * decimalUnit),
      -- 2024-12-05, 21:45 UTC
      (Ledger.POSIXTime 1733435100000, 4_795 * decimalUnit),
      -- 2025-07-13, 21:45 UTC
      (Ledger.POSIXTime 1752443100000, 4_794 * decimalUnit),
      -- 2025-12-05, 21:45 UTC
      (Ledger.POSIXTime 1764971100000, 5_993 * decimalUnit),
      -- 2026-12-05, 21:45 UTC
      (Ledger.POSIXTime 1796507100000, 7_191 * decimalUnit)
    ]
  )

-------------
-- Functions
-------------

-- | Calculate total vested amount for a given schedule
-- based on the current time.
-- The vested amount increases every 5 days = 1 Cardano epoch
-- until it reaches the maximum amount available.
--
-- Note:
--  For the calculation to work properly the difference between dates
--  in `VestingSchedule` has to be a multiple of a Cardano epoch.
{-# INLINEABLE calculateVestedPerSchedule #-}
calculateVestedPerSchedule :: VestingSchedule -> POSIXTime -> Integer
calculateVestedPerSchedule (maxUnlockable, schedule) (POSIXTime currentTime) =
  min maxUnlockable (go schedule)
  where
    -- 5 days = 1 Cardano epoch
    vestingFreq :: Integer
    vestingFreq = 5 * getPOSIXTime oneDay

    go [] = 0
    go [(POSIXTime date, amt)] =
      {-
         If the current time is earlier than the first vesting period,
         the vested INDY is zero.

         Otherwise, the vested INDY increases by the given amount every epoch
         after the given date.

         The + 1 indicates that the first vest happens from the given date
         (as opposed to having to wait for an epoch to complete)
      -}
      Utils.zeroNegatives $
        (((currentTime - date) `divideInteger` vestingFreq) + 1) * amt
    go ((POSIXTime date1, amt1) : x2@(POSIXTime date2, _) : xs)
      {-
         If the current time is past the first period in the schedule,
         at least all INDY from the first period is vested, and can proceed
         to compute the vested amount for remaining periods.
      -}
      | currentTime >= date2 =
          ((date2 - date1) `divideInteger` vestingFreq) * amt1
            + go (x2 : xs)
      -- Otherwise, the remaining periods can be ignored.
      | otherwise =
          Utils.zeroNegatives $
            (((currentTime - date1) `divideInteger` vestingFreq) + 1) * amt1

-- | Aggregates the vested amount for each vesting schedule to obtain the
-- total amount of INDY vested.
--
-- Following Indigo's tokenomics, rewards are distributed to Stability Pool
-- stakers, DEX Liquidity providers and Governance voters, having each group
-- a different vesting schedule.
{-# INLINEABLE calculateTotalVested #-}
calculateTotalVested :: POSIXTime -> Integer
calculateTotalVested currentTime =
  calculateVestedPerSchedule spDistributionSchedule currentTime
    + calculateVestedPerSchedule govDistributionSchedule currentTime
    + calculateVestedPerSchedule liqDistributionSchedule currentTime

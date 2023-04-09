module Test.Plutip.Options (
  TraceOption (..),
) where

import BotPlutusInterface.Types (LogContext)

-- -- | Extra options for `assertExecutionWith`.
-- data TraceOptions
--   -- | Display logs collected by BPI during contract execution.
--   = Tracing
--   -- | Like `Tracing` but choose which context to display.
--   | TracingButOnlyContext
--       LogContext -- ^ Can be `ContractLog` or `BpiLog` for internal bpi requests/responses
--   -- | Display transaction execution budgets.
--   | BudgetCounting

data TraceOption
  -- | Display logs collected by BPI during contract execution.
  = DisplayAllTrace
  -- | Like `Tracing` but choose which context to display.
  | DisplayOnlyFromContext
      LogContext -- ^ Can be `ContractLog` or `BpiLog` for internal bpi requests/responses
  deriving stock (Eq, Show)

data BudgetsOption = BudgetsOption
  deriving stock (Eq, Show)

data TracingOption
  = TracingOption (Maybe TraceOption) (Maybe BudgetsOption)
  deriving stock (Eq, Show)

-- On both axis take Just prefering right just.
instance Semigroup TracingOption where 
  (TracingOption t1 b1) <> (TracingOption t2 b2) = TracingOption (t1 <>> t2) (b1 <>> b2)
    where 
      _ <>> Just b = Just b
      a <>> Nothing = a



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

instance Monoid 


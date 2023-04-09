module Test.Plutip.Options (
  TraceOption (..),
) where

import BotPlutusInterface.Types (LogContext)

-- | Extra options for `assertExecutionWith`.
data TraceOption
  -- | Display logs collected by BPI during contract execution.
  = Tracing
  -- | Like `Tracing` but choose which context to display.
  | TracingButOnlyContext
      LogContext -- ^ Can be `ContractLog` or `BpiLog` for internal bpi requests/responses
  -- | Display transaction execution budgets.
  | BudgetCounting

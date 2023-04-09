module Test.Plutip.Options (
  TraceOption (..),
) where

import BotPlutusInterface.Types (LogContext, LogLevel)

-- | Extra options for `assertExecutionWith`.
data TraceOption
  -- | Display logs collected by BPI during contract execution.
  = Tracing
  -- | Like `Tracing` but choose which context to display and starting from which log level.
  | TracingButOnlyContext
      LogContext -- ^ Can be `ContractLog` or `BpiLog` for internal bpi requests/responses
      LogLevel   -- ^ Lower bound on log level, can be . 
  -- | Display transaction execution budgets.
  | BudgetCounting

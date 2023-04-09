module Test.Plutip.Options (
  TraceOption (..),
) where

import BotPlutusInterface.Types (LogContext)

-- | Extra options for `assertExecutionWith`.
data TraceOption
  = -- | Display logs collected by BPI during contract execution.
    ShowTrace
  | -- | Can be `ContractLog` or `BpiLog` for internal bpi requests/responses
    -- | Display transaction execution budgets.
    ShowTraceButOnlyContext
      LogContext
  | ShowBudgets

-- | TraceOption stripped to what LogsReport wants to know.
data LogsReportOption
  = -- | Display logs collected by BPI during contract execution.
    DisplayAllTrace
  | -- | Like `ShowTrace` but choose which context to display.
    DisplayOnlyFromContext
      LogContext
  deriving stock (Eq, Show)

-- | TraceOption stripped to what StatReport wants to know.
data BudgetsOption = BudgetsOption
  deriving stock (Eq, Show)

data TracingOption
  = TracingOption (Maybe LogsReportOption) (Maybe BudgetsOption)
  deriving stock (Eq, Show)

-- | On both axis take Just prefering the right just.
instance Semigroup TracingOption where
  (TracingOption t1 b1) <> (TracingOption t2 b2) = TracingOption (t1 <>> t2) (b1 <>> b2)
    where
      _ <>> Just b = Just b
      a <>> Nothing = a

tracing :: TracingOption
tracing = TracingOption (Just DisplayAllTrace) Nothing

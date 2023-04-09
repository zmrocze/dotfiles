{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Functor law" #-}

module Test.Plutip.Internal.BotPlutusInterface.Run (
  defCollateralSize,
  runContractWithLogLvl,
  runContract,
  runContract_,
) where

import Control.Concurrent.STM (newTVarIO, readTVarIO)
import Control.Exception (try)
import Control.Monad (void)
import Control.Monad.Catch (SomeException)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (ToJSON, eitherDecodeFileStrict')
import Data.Either.Combinators (fromRight)
import Data.Kind (Type)
import Data.Row (Row)
import Data.Text qualified as Text
import Data.UUID.V4 qualified as UUID
import Test.Plutip.Config (PlutipConfig (budgetMultiplier))
import Test.Plutip.Internal.BotPlutusInterface.Setup qualified as BIS
import Test.Plutip.Internal.BotPlutusInterface.Wallet (BpiWallet (walletPkh))
import Test.Plutip.Internal.Types (
  ClusterEnv (chainIndexUrl, networkId, plutipConf),
  ExecutionResult (ExecutionResult),
  FailureReason (CaughtException, ContractExecutionError),
 )

-- | default collateral size that's to be used as collateral.
defCollateralSize :: Integer
defCollateralSize = 10_000_000

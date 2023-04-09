{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

-- | Helpers based on Cardano.Api (do not use `cardano-cli` executable)
module Test.Plutip.Tools.CardanoApi (
  currentBlock,
  utxosAtAddress,
  utxosAtAddresses,
  queryProtocolParams,
  queryTip,
  awaitUtxosNumber,
  -- plutusValueFromAddress,
  CardanoApiError,
  AwaitWalletFundedError (AwaitingCapiError, AwaitingTimeoutError)
  ) where

import Cardano.Api qualified as C
import Cardano.Api.Shelley (ProtocolParameters, UTxO (UTxO))
  -- , txOutValueToValue)
import Cardano.Launcher.Node (nodeSocketFile)
import Cardano.Slotting.Slot (WithOrigin)
import Test.Plutip.Internal.Cluster (RunningNode (RunningNode))

import Control.Exception (Exception)
import Control.Arrow (right)
import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader (ask), ReaderT)
import Control.Retry (constantDelay, limitRetries, recoverAll, retrying)
import Data.Either (fromRight)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Time (NominalDiffTime, nominalDiffTimeToSeconds)
import GHC.Generics (Generic)
-- import Ledger (Value)
-- import Ledger.Tx.CardanoAPI (fromCardanoValue)
import Ouroboros.Consensus.HardFork.Combinator.AcrossEras (EraMismatch)
import Test.Plutip.Internal.Types (ClusterEnv (runningNode, plutipConf))
import Test.Plutip.Internal.Cluster.Extra.Types (ExtraConfig (ecSlotLength))
import UnliftIO (throwString)
import Test.Plutip.Config (PlutipConfig(extraConfig))
-- import Plutus.V1.Ledger.Value (Value)

newtype CardanoApiError
  = SomeError String
  deriving stock (Eq, Show, Generic)

instance Exception CardanoApiError

-- | Get current block using `Cardano.Api` library
currentBlock :: ClusterEnv -> IO (Either _ (WithOrigin C.BlockNo))
currentBlock (runningNode -> rn) = do
  let query = C.QueryChainBlockNo
      info = connectionInfo rn
  C.queryNodeLocalState info Nothing query

utxosAtAddresses :: ClusterEnv -> [C.AddressAny] -> IO (Either CardanoApiError (C.UTxO C.BabbageEra))
utxosAtAddresses (runningNode -> rn) addrs = do
  flattenQueryResult <$> C.queryNodeLocalState info Nothing query
  where
    info = connectionInfo rn
    query =
      shellyBasedBabbageQuery
        (C.QueryUTxO $ C.QueryUTxOByAddress $ Set.fromList addrs)

utxosAtAddress :: ClusterEnv -> C.AddressAny -> IO (Either CardanoApiError (C.UTxO C.BabbageEra))
utxosAtAddress cenv = utxosAtAddresses cenv . pure

queryProtocolParams :: ClusterEnv -> IO (Either CardanoApiError ProtocolParameters)
queryProtocolParams (runningNode -> rn) =
  flattenQueryResult <$> C.queryNodeLocalState info Nothing query
  where
    info = connectionInfo rn
    query = shellyBasedBabbageQuery C.QueryProtocolParameters

connectionInfo :: RunningNode -> C.LocalNodeConnectInfo C.CardanoMode
connectionInfo (RunningNode socket _ _ _) =
  C.LocalNodeConnectInfo
    (C.CardanoModeParams (C.EpochSlots 21600))
    C.Mainnet
    (nodeSocketFile socket)

queryTip :: RunningNode -> IO C.ChainTip
queryTip = C.getLocalChainTip . connectionInfo

shellyBasedBabbageQuery ::
  C.QueryInShelleyBasedEra C.BabbageEra result ->
  C.QueryInMode C.CardanoMode (Either EraMismatch result)
shellyBasedBabbageQuery =
  C.QueryInEra C.BabbageEraInCardanoMode
    . C.QueryInShelleyBasedEra C.ShelleyBasedEraBabbage

flattenQueryResult ::
  (Show e1, Show e2, Show b) =>
  Either e1 (Either e2 b) ->
  Either CardanoApiError b
flattenQueryResult = \case
  Right (Right res) -> Right res
  err -> Left $ SomeError (show err)

data AwaitWalletFundedError
  = AwaitingCapiError CardanoApiError
  | AwaitingTimeoutError

instance Show AwaitWalletFundedError where
  show (AwaitingCapiError (SomeError e)) = e
  show AwaitingTimeoutError = "Awaiting funding transaction timed out."

-- | Wait till specified addresses are funded, 
-- by checking if they collectively own an expected number of utxos.
-- Performs 60 cardano-node queries with `retryDelay` seconds between tries.
awaitUtxosNumber ::
  ClusterEnv ->
  [C.AddressAny] ->
  Int ->
  IO (Either AwaitWalletFundedError ())
awaitUtxosNumber cenv addrs utxosNum =
  toErrorMsg <$> retrying policy checkResponse action
  
  where
    retryDelay = ecSlotLength $ extraConfig $ plutipConf cenv
    delay = truncate $ nominalDiffTimeToSeconds retryDelay * 1000000
    policy = constantDelay delay <> limitRetries 60

    action _ = right (\x -> utxosNum == Map.size (C.unUTxO x)) <$> utxosAtAddresses cenv addrs

    checkResponse _ = return . fromRight False

    toErrorMsg = \case
      Left e -> Left $ AwaitingCapiError e
      Right noUtxos ->
        if noUtxos
          then Left AwaitingTimeoutError
          else Right ()

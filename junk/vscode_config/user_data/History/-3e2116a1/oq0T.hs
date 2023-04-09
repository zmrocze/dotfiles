-- | Helpers based on Cardano.Api (do not use `cardano-cli` executable)
module Test.Plutip.Tools.CardanoApi (
  currentBlock,
  utxosAtAddress,
  queryProtocolParams,
  queryTip,
  awaitAddressFunded,
) where

import Cardano.Api qualified as C
import Cardano.Api.Shelley (ProtocolParameters, UTxO (UTxO))
import Cardano.Launcher.Node (nodeSocketFile)
import Cardano.Slotting.Slot (WithOrigin)
import Test.Plutip.Internal.Cluster (RunningNode (RunningNode))

import Control.Exception (Exception)
import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader (ask), ReaderT)
import Control.Retry (constantDelay, limitRetries, recoverAll)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Time (NominalDiffTime, nominalDiffTimeToSeconds)
import GHC.Generics (Generic)
import Ouroboros.Consensus.HardFork.Combinator.AcrossEras (EraMismatch)
import Ouroboros.Network.Protocol.LocalStateQuery.Type (AcquireFailure)
import Test.Plutip.Internal.Types (ClusterEnv (runningNode))
import UnliftIO (throwString)

newtype CardanoApiError
  = SomeError String
  deriving stock (Eq, Show, Generic)

instance Exception CardanoApiError

-- | Get current block using `Cardano.Api` library
currentBlock :: ClusterEnv -> IO (Either AcquireFailure (WithOrigin C.BlockNo))
currentBlock (runningNode -> rn) = do
  let query = C.QueryChainBlockNo
      info = connectionInfo rn
  C.queryNodeLocalState info Nothing query

utxosAtAddress :: ClusterEnv -> C.AddressAny -> IO (Either CardanoApiError (C.UTxO C.BabbageEra))
utxosAtAddress (runningNode -> rn) addr = do
  flattenQueryResult <$> C.queryNodeLocalState info Nothing query
  where
    info = connectionInfo rn
    query =
      shellyBasedBabbageQuery
        (C.QueryUTxO $ C.QueryUTxOByAddress (Set.singleton addr))

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

-- | Waits till specified address is funded using `CardanoApi` query.
-- Performs 60 tries with `retryDelay` seconds between tries.
awaitAddressFunded ::
  (MonadIO m, MonadMask m) =>
  C.AddressAny ->
  NominalDiffTime ->
  ReaderT ClusterEnv m ()
awaitAddressFunded addr retryDelay = do
  cEnv <- ask
  recoverAll policy $ \_ -> do
    utxo <- liftIO $ utxosAtAddress cEnv addr
    checkUtxo utxo
  where
    delay = truncate $ nominalDiffTimeToSeconds retryDelay * 1000000
    policy = constantDelay delay <> limitRetries 60

    checkUtxo = \case
      Left e ->
        throwString $
          "Failed to get UTxO from address via cardano API query: "
            <> show e
      Right (UTxO utxo')
        | Map.null utxo' ->
          throwString "No UTxOs returned by cardano API query for address"
      _ -> pure ()

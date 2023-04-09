
module Spec.ClusterStartup where
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase, assertFailure)
import Plutip.Cluster (withCluster)
import Data.Default (Default(def))
import Plutip.Tools.CardanoApi (currentBlock, AwaitWalletFundedError (AwaitingCapiError, AwaitingTimeoutError))
import Cardano.Api (UTxO(unUTxO))
import qualified Cardano.Api as Capi
import qualified Data.Map as Map
import qualified Data.Set as Set

test = testGroup "Cluster startup" [
  test1,
  test2
  ]

test1 :: TestTree
test1 = testCase "Cluster starts up" $ do
  withCluster def $ \cenv -> do
    res <- currentBlock cenv
    case res of
      Left _ -> assertFailure "Failed to query the cluster node for block number."
      Right _ -> pure ()

test2 :: TestTree
test2 = testCase "Funded cluster starts up funded" $ do
  withCluster def distr $ \cenv keys -> do
    res <- zipWithM (utxosAtAddress cenv)  distr (cardanoMainnetAddress <$> keys)
    case sequence_ res of
      Left (AwaitingCapiError err) -> assertFailure $ "Failed to query the cluster node for utxos: " <> show err
      Left AwaitingTimeoutError -> assertFailure $ "Queried utxo distribution is incorrect."
      Right _ -> pure ()

  where
    distr :: [[Capi.Lovelace]]
    distr = [[1], [2, 22], [3, 33, 333]]
    eqDistr ds1 queried = 
      let ds2 = map (\case Capi.TxOut _ value _ _ -> Capi.txOutValueToLovelace value) $ Map.elems $ unUTxO queried
        in Set.fromList ds1 == Set.fromList ds2 && length ds1 == length ds2
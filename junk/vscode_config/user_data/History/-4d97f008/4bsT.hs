module BotPlutusInterface.Collateral (
  getInMemCollateral,
  setInMemCollateral,
  filterCollateral,
  mkCollateralTx,
  removeCollateral
) where

import BotPlutusInterface.Types (ContractEnvironment (ceCollateral), PABConfig (pcOwnPubKeyHash), collateralValue, unCollateralVar, CollateralUtxo (CollateralUtxo))
import Cardano.Prelude (Void)
import Control.Concurrent.STM (atomically, modifyTVar', readTVarIO)
import Ledger (PaymentPubKeyHash (PaymentPubKeyHash), TxOutRef)
import Ledger.Constraints qualified as Constraints
import Prelude
import Plutus.ChainIndex (Page(..), ChainIndexTx)
import Data.Map (Map)

getInMemCollateral :: ContractEnvironment w -> IO (Maybe CollateralUtxo)
getInMemCollateral = readTVarIO . unCollateralVar . ceCollateral

setInMemCollateral :: ContractEnvironment w -> CollateralUtxo -> IO ()
setInMemCollateral cEnv txOutRef = do
  let cVar = unCollateralVar $ ceCollateral cEnv
  atomically $ modifyTVar' cVar (const (Just txOutRef))

mkCollateralTx :: PABConfig -> Either Constraints.MkTxError Constraints.UnbalancedTx
mkCollateralTx pabConf = Constraints.mkTx @Void mempty txc
  where
    txc = Constraints.mustPayToPubKey (PaymentPubKeyHash $ pcOwnPubKeyHash pabConf) (collateralValue pabConf)

filterCollateral :: CollateralUtxo -> [TxOutRef] -> [TxOutRef]
filterCollateral (CollateralUtxo collateralTxOutRef) = filter (/= collateralTxOutRef)

-- | Removes collateral utxo from the UtxoResponse page. Receives `Nothing` if Collateral uninitialized.
removeCollateralFromPage :: Maybe CollateralUtxo -> Page TxOutRef -> Page TxOutRef
removeCollateral = \case
  Nothing -> id
  (Just txOutRef) -> \page -> page {pageItems = filterCollateral txOutRef (pageItems page)}

removeCollateralFromMap :: Maybe CollateralUtxo -> Map TxOutRef ChainIndexTx -> Map TxOutRef ChainIndexTx
removeCollateralFromMap = \case
  
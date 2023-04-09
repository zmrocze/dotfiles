module BotPlutusInterface.Collateral (
  getInMemCollateral,
  setInMemCollateral,
  filterCollateral,
  mkCollateralTx,
) where

import BotPlutusInterface.Types (ContractEnvironment (ceCollateral), PABConfig (pcOwnPubKeyHash), collateralValue, unCollateral, Collateral (Collateral))
import Cardano.Prelude (Void)
import Control.Concurrent.STM (atomically, modifyTVar', readTVarIO)
import Ledger (PaymentPubKeyHash (PaymentPubKeyHash), TxOutRef)
import Ledger.Constraints qualified as Constraints
import Prelude

getInMemCollateral :: ContractEnvironment w -> IO (Maybe TxOutRef)
getInMemCollateral = readTVarIO . unCollateral . ceCollateral

setInMemCollateral :: ContractEnvironment w -> TxOutRef -> IO ()
setInMemCollateral cEnv txOutRef = do
  let cVar = unCollateral $ ceCollateral cEnv
  atomically $ modifyTVar' cVar (const (Just txOutRef))

mkCollateralTx :: PABConfig -> Either Constraints.MkTxError Constraints.UnbalancedTx
mkCollateralTx pabConf = Constraints.mkTx @Void mempty txc
  where
    txc = Constraints.mustPayToPubKey (PaymentPubKeyHash $ pcOwnPubKeyHash pabConf) (collateralValue pabConf)

filterCollateral :: Collateral -> [TxOutRef] -> [TxOutRef]
filterCollateral (Collateral collateral) = filter (/= ) 

-- | Removes given utxo from the UtxoResponse. Used to remove collateral utxo.
removeUtxo :: Maybe TxOutRef -> Page TxOutRef -> Page TxOutRef
removeUtxo = \case
  Nothing -> id
  (Just txOutRef) -> \page -> page {pageItems = filter (/= txOutRef) (pageItems page)}
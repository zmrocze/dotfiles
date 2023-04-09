module BotPlutusInterface.Collateral (
  getInMemCollateral,
  setInMemCollateral,
  filterCollateral,
  mkCollateralTx,
  removeCollateralFromPage,
  removeCollateralFromMap,
) where

import BotPlutusInterface.Types (
  CollateralUtxo (CollateralUtxo),
  ContractEnvironment (ceCollateral),
  PABConfig (pcOwnPubKeyHash),
  collateralValue,
  unCollateralVar, pcOwnStakePubKeyHash
 )
import Cardano.Prelude (Void)
import Control.Concurrent.STM (atomically, readTVarIO, writeTVar)
import Data.Kind (Type)
import Data.Map (Map)
import Data.Map qualified as Map
import Ledger (ChainIndexTxOut, PaymentPubKeyHash (PaymentPubKeyHash), TxOutRef)
import Ledger.Constraints qualified as Constraints
import Plutus.ChainIndex (Page (pageItems))
import Prelude

getInMemCollateral :: forall (w :: Type). ContractEnvironment w -> IO (Maybe CollateralUtxo)
getInMemCollateral = readTVarIO . unCollateralVar . ceCollateral

setInMemCollateral :: forall (w :: Type). ContractEnvironment w -> CollateralUtxo -> IO ()
setInMemCollateral cEnv txOutRef = do
  let cVar = unCollateralVar $ ceCollateral cEnv
  atomically $ writeTVar cVar (Just txOutRef)

mkCollateralTx :: PABConfig -> Either Constraints.MkTxError Constraints.UnbalancedTx
mkCollateralTx pabConf = Constraints.mkTx @Void mempty txc
  where
    txc :: Constraints.TxConstraints Void Void
    txc =
      maybe
        Constraints.mustPayToPubKey
        (flip Constraints.mustPayToPubKeyAddress)
        (pcOwnStakePubKeyHash pabConf)
      (PaymentPubKeyHash $ pcOwnPubKeyHash pabConf)
      (collateralValue pabConf)

    -- mustPayToAddress pkh mskh vl = Constraints.singleton (Constraints.MustPayToPubKeyAddress pkh mskh Nothing Nothing vl)

filterCollateral :: CollateralUtxo -> [TxOutRef] -> [TxOutRef]
filterCollateral (CollateralUtxo collateralTxOutRef) = filter (/= collateralTxOutRef)

-- | Removes collateral utxo from the UtxoResponse page. Receives `Nothing` if Collateral uninitialized.
removeCollateralFromPage :: Maybe CollateralUtxo -> Page TxOutRef -> Page TxOutRef
removeCollateralFromPage = \case
  Nothing -> id
  Just txOutRef -> \page -> page {pageItems = filterCollateral txOutRef (pageItems page)}

removeCollateralFromMap :: Maybe CollateralUtxo -> Map TxOutRef ChainIndexTxOut -> Map TxOutRef ChainIndexTxOut
removeCollateralFromMap = \case
  Nothing -> id
  Just (CollateralUtxo collateral) -> Map.filterWithKey (\oref _ -> collateral /= oref)

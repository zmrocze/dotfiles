{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NamedFieldPuns #-}

module BotPlutusInterface.Balance (
  BalanceConfig (BalanceConfig, bcHasScripts, bcSeparateChange),
  balanceTxStep,
  balanceTxIO,
  balanceTxIO',
  defaultBalanceConfig,
  txUsesScripts,
  withFee,
) where

import BotPlutusInterface.BodyBuilder qualified as BodyBuilder
import BotPlutusInterface.CardanoCLI qualified as CardanoCLI
import BotPlutusInterface.CardanoNode.Effects (NodeQuery (UtxosAt))
import BotPlutusInterface.CoinSelection (selectTxIns)
import BotPlutusInterface.Collateral (removeCollateralFromMap)
import BotPlutusInterface.Effects (
  PABEffect,
  createDirectoryIfMissingCLI,
  getInMemCollateral,
  posixTimeRangeToContainedSlotRange,
  printBpiLog,
  queryNode,
 )
import BotPlutusInterface.Files (DummyPrivKey, unDummyPrivateKey)
import BotPlutusInterface.Files qualified as Files
import BotPlutusInterface.Types (
  CollateralUtxo (collateralTxOutRef),
  LogLevel (Debug),
  LogType (TxBalancingLog),
  PABConfig,
  collateralTxOutRef,
 )
import Cardano.Api (ExecutionUnitPrices (ExecutionUnitPrices))
import Cardano.Api.Shelley (ProtocolParameters (protocolParamPrices))
import Control.Lens (folded, to, (^..))
import Control.Monad (foldM, void)
import Control.Monad.Freer (Eff, Member)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Either (EitherT, firstEitherT, hoistEither, newEitherT, runEitherT)
import Control.Monad.Trans.Except (throwE)
import Data.Bifunctor (bimap)
import Data.Coerce (coerce)
import Data.Kind (Type)
import Data.List qualified as List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, mapMaybe, isJust)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Real (Ratio ((:%)))
import Ledger qualified
import Ledger.Ada qualified as Ada
import Ledger.Address (Address (..))
import Ledger.Constraints.OffChain (UnbalancedTx (..))
import Ledger.Crypto (PubKeyHash)
import Ledger.Interval (
  Extended (Finite, NegInf, PosInf),
  Interval (Interval),
  LowerBound (LowerBound),
  UpperBound (UpperBound),
 )
import Ledger.Time (POSIXTimeRange)
import Ledger.Tx (
  Tx (..),
  TxIn (..),
  TxInType (..),
  TxOut (..),
  TxOutRef (..),
 )
import Ledger.Tx qualified as Tx
import Ledger.Tx.CardanoAPI (CardanoBuildTx)
import Ledger.Value (Value)
import Ledger.Value qualified as Value
import Plutus.V1.Ledger.Api (
  CurrencySymbol (..),
  TokenName (..),
 )
import Prettyprinter (pretty, viaShow, (<+>))
import Prelude

-- Config for balancing a `Tx`.
data BalanceConfig = BalanceConfig
  { -- | This field represents whether the current `Tx` that needs to be balanced uses scripts.
    bcHasScripts :: Bool
  , -- | This field represents whether the ada change should be in separate UTxO.
    bcSeparateChange :: Bool
  }
  deriving stock (Show, Eq)

defaultBalanceConfig :: BalanceConfig
defaultBalanceConfig = BalanceConfig {bcHasScripts = False, bcSeparateChange = False}

{- | Collect necessary tx inputs and collaterals, add minimum lovelace values and balance non ada
     assets. `balanceTxIO` calls `balanceTxIO' with default `BalanceConfig`.
-}
balanceTxIO ::
  forall (w :: Type) (effs :: [Type -> Type]).
  (Member (PABEffect w) effs) =>
  PABConfig ->
  PubKeyHash ->
  UnbalancedTx ->
  Eff effs (Either Text Tx)
balanceTxIO = balanceTxIO' @w defaultBalanceConfig

-- | `balanceTxIO'` is more flexible version of `balanceTxIO`, this let us specify custom `BalanceConfig`.
balanceTxIO' ::
  forall (w :: Type) (effs :: [Type -> Type]).
  (Member (PABEffect w) effs) =>
  BalanceConfig ->
  PABConfig ->
  PubKeyHash ->
  UnbalancedTx ->
  Eff effs (Either Text Tx)
balanceTxIO' balanceCfg pabConf ownPkh unbalancedTx =
  runEitherT $
    do
      (utxos, mcollateral) <- newEitherT $ utxosAndCollateralAtAddress @w balanceCfg pabConf changeAddr
      privKeys <- newEitherT $ Files.readPrivateKeys @w pabConf

      let utxoIndex :: Map TxOutRef TxOut
          utxoIndex = fmap Tx.toTxOut utxos <> unBalancedTxUtxoIndex unbalancedTx

          utxoIndexD, utxoIndexS :: Map TxOutRef TxOut
          (utxoIndexD, utxoIndexS) = splitUtxos utxoIndex

          requiredSigs :: [PubKeyHash]
          requiredSigs = map Ledger.unPaymentPubKeyHash $ Set.toList (unBalancedTxRequiredSignatories unbalancedTx)

      lift $ printBpiLog @w (Debug [TxBalancingLog]) $ viaShow utxoIndex

      -- We need this folder on the CLI machine, which may not be the local machine
      lift $ createDirectoryIfMissingCLI @w False (Text.unpack "pcTxFileDir")

      tx <-
        newEitherT $
          addValidRange @w
            (unBalancedTxValidityTimeRange unbalancedTx)
            (unBalancedTxTx unbalancedTx)

      -- Adds required collaterals in the `Tx`, if `bcHasScripts`
      -- is true. Also adds signatures for fee calculation
      preBalancedTx <-
        if bcHasScripts balanceCfg
          then
            maybe
              (throwE "Tx uses script but no collateral was provided.")
              (hoistEither . addSignatories ownPkh privKeys requiredSigs . flip addTxCollaterals tx)
              mcollateral
          else hoistEither $ addSignatories ownPkh privKeys requiredSigs tx

      -- Balance the tx
<<<<<<< HEAD
      balancedTx <- balanceTxLoop utxoIndex privKeys preBalancedTx
=======
      -- (balancedTx, minUtxos) <- balanceTxLoop utxoIndex privKeys [] preBalancedTx
      (balancedTx', minUtxos) <- balanceTxLoop utxoIndexS privKeys [] preBalancedTx

      let txOuts1 = txOutputs balancedTx'
          txOuts2 = txOuts1 <> Map.elems utxoIndexD
          balancedTx = balancedTx' {txOutputs = txOuts2}
      
>>>>>>> c9b7cbc (Partitions UTxOs into those with/without datums.)

      -- Get current Ada change
      let adaChange = getAdaChange utxoIndex balancedTx
          bTx = balanceTxLoop utxoIndex privKeys (addOutput changeAddr balancedTx)

      -- Checks if there's ada change left, if there is then we check
      -- if `bcSeparateChange` is true, if this is the case then we create a new UTxO at
      -- the changeAddr.
      balancedTxWithChange <-
        case adaChange /= 0 of
          True | bcSeparateChange balanceCfg || not (hasChangeUTxO changeAddr balancedTx) -> bTx
          _ -> pure balancedTx

      -- Get the updated change, add it to the tx
      let finalAdaChange = getAdaChange utxoIndex balancedTxWithChange
          fullyBalancedTx = addAdaChange balanceCfg changeAddr finalAdaChange balancedTxWithChange
          txInfoLog =
            printBpiLog @w (Debug [TxBalancingLog]) $
              "UnbalancedTx TxInputs: "
                <+> pretty (length $ txInputs preBalancedTx)
                <+> "UnbalancedTx TxOutputs: "
                <+> pretty (length $ txOutputs preBalancedTx)
                <+> "TxInputs: "
                <+> pretty (length $ txInputs fullyBalancedTx)
                <+> "TxOutputs: "
                <+> pretty (length $ txOutputs fullyBalancedTx)

      lift txInfoLog

      -- finally, we must update the signatories
      hoistEither $ addSignatories ownPkh privKeys requiredSigs fullyBalancedTx
  where
    changeAddr :: Address
    changeAddr = Ledger.pubKeyHashAddress (Ledger.PaymentPubKeyHash ownPkh) pabConf.pcOwnStakePubKeyHash

    balanceTxLoop ::
      Map TxOutRef TxOut ->
      Map PubKeyHash DummyPrivKey ->
      Tx ->
      EitherT Text (Eff effs) Tx
    balanceTxLoop utxoIndex privKeys tx = do
      void $ lift $ Files.writeAll @w pabConf tx

      -- Calculate fees by pre-balancing the tx, building it, and running the CLI on result
      txWithoutFees <-
        newEitherT $ balanceTxStep @w balanceCfg utxoIndex changeAddr $ tx `withFee` 0

      exBudget <- newEitherT $ BodyBuilder.buildAndEstimateBudget @w pabConf privKeys txWithoutFees

      nonBudgettedFees <- newEitherT $ CardanoCLI.calculateMinFee @w pabConf txWithoutFees

      let fees = nonBudgettedFees + getBudgetPrice (getExecutionUnitPrices pabConf) exBudget

      lift $ printBpiLog @w (Debug [TxBalancingLog]) $ "Fees:" <+> pretty fees

      -- Rebalance the initial tx with the above fees
      balancedTx <- newEitherT $ balanceTxStep @w balanceCfg utxoIndex changeAddr $ tx `withFee` fees

      if balancedTx == tx
        then pure balancedTx
        else balanceTxLoop utxoIndex privKeys balancedTx

-- `utxosAndCollateralAtAddress` returns all the utxos that can be used as an input of a `Tx`,
-- i.e. we filter out `CollateralUtxo` present at the user's address, so it can't be used as input of a `Tx`.
utxosAndCollateralAtAddress ::
  forall (w :: Type) (effs :: [Type -> Type]).
  (Member (PABEffect w) effs) =>
  BalanceConfig ->
  PABConfig ->
  Address ->
  Eff effs (Either Text (Map TxOutRef Tx.ChainIndexTxOut, Maybe CollateralUtxo))
utxosAndCollateralAtAddress balanceCfg _pabConf changeAddr =
  runEitherT $ do
    utxos <- firstEitherT (Text.pack . show) $ newEitherT $ queryNode @w (UtxosAt changeAddr)
    inMemCollateral <- lift $ getInMemCollateral @w

    -- check if `bcHasScripts` is true, if this is the case then we search of
    -- collateral UTxO in the environment, if such collateral is not present we throw Error.
    if bcHasScripts balanceCfg
      then
        maybe
          ( throwE $
              "The given transaction uses script, but there's no collateral provided."
                <> "This usually means that, we failed to create Tx and update our ContractEnvironment."
          )
          (const $ pure (removeCollateralFromMap inMemCollateral utxos, inMemCollateral))
          inMemCollateral
      else pure (removeCollateralFromMap inMemCollateral utxos, Nothing)

hasChangeUTxO :: Address -> Tx -> Bool
hasChangeUTxO changeAddr tx =
  any check $ txOutputs tx
  where
    check :: TxOut -> Bool
    check txOut =
      Tx.txOutAddress txOut == changeAddr

getExecutionUnitPrices :: PABConfig -> ExecutionUnitPrices
getExecutionUnitPrices pabConf =
  fromMaybe (ExecutionUnitPrices 0 0) $
    pabConf.pcProtocolParams >>= protocolParamPrices

getBudgetPrice :: ExecutionUnitPrices -> Ledger.ExBudget -> Integer
getBudgetPrice (ExecutionUnitPrices cpuPrice memPrice) (Ledger.ExBudget cpuUsed memUsed) =
  round cpuCost + round memCost
  where
    cpuCost = cpuPrice `multRational` (toInteger @Ledger.SatInt $ coerce cpuUsed)
    memCost = memPrice `multRational` (toInteger @Ledger.SatInt $ coerce memUsed)

multRational :: Rational -> Integer -> Rational
multRational (num :% denom) s = (s * num) :% denom

withFee :: Tx -> Integer -> Tx
withFee tx fee = tx {txFee = Ada.lovelaceValueOf fee}

balanceTxStep ::
  forall (w :: Type) (effs :: [Type -> Type]).
  Member (PABEffect w) effs =>
  BalanceConfig ->
  Map TxOutRef TxOut ->
  Address ->
  Tx ->
  Eff effs (Either Text Tx)
balanceTxStep balanceCfg utxos changeAddr tx =
  runEitherT $
    (newEitherT . balanceTxIns @w utxos) tx
      >>= hoistEither . handleNonAdaChange balanceCfg changeAddr utxos

-- | Get change value of a transaction, taking inputs, outputs, mint and fees into account
getChange :: Map TxOutRef TxOut -> Tx -> Value
getChange utxos tx =
  let fees = lovelaceValue $ txFee tx
      txInRefs = map Tx.txInRef $ txInputs tx
      inputValue = mconcat $ map Tx.txOutValue $ mapMaybe (`Map.lookup` utxos) txInRefs
      outputValue = mconcat $ map Tx.txOutValue $ txOutputs tx
      nonMintedOutputValue = outputValue `minus` txMint tx
      change = (inputValue `minus` nonMintedOutputValue) `minus` Ada.lovelaceValueOf fees
   in change

lovelaceValue :: Value -> Integer
lovelaceValue = flip Value.assetClassValueOf $ Value.assetClass "" ""

getAdaChange :: Map TxOutRef TxOut -> Tx -> Integer
getAdaChange utxos = lovelaceValue . getChange utxos

getNonAdaChange :: Map TxOutRef TxOut -> Tx -> Value
getNonAdaChange utxos = Ledger.noAdaValue . getChange utxos

hasDatum :: TxOut -> Bool
hasDatum = isJust . txOutDatumHash

-- | Split UTxOs into ones that have data
-- and those that don't.
splitUtxos :: Map TxOutRef TxOut -> (Map TxOutRef TxOut, Map TxOutRef TxOut)
splitUtxos = Map.partition hasDatum

-- -- | Add min lovelaces to each tx output
-- addLovelaces :: [(TxOut, Integer)] -> Tx -> Tx
-- addLovelaces minLovelaces tx =
--   let lovelacesAdded =
--         map
--           ( \txOut ->
--               let outValue = txOutValue txOut
--                   lovelaces = Ada.getLovelace $ Ada.fromValue outValue
--                   minUtxo = fromMaybe 0 $ lookup txOut minLovelaces
--                in txOut
--                     { txOutValue =
--                         outValue <> Ada.lovelaceValueOf (max 0 (minUtxo - lovelaces))
--                     }
--           )
--           $ txOutputs tx
--    in tx {txOutputs = lovelacesAdded}

balanceTxIns ::
  forall (w :: Type) (effs :: [Type -> Type]).
  Member (PABEffect w) effs =>
  Map TxOutRef TxOut ->
  Tx ->
  Eff effs (Either Text Tx)
balanceTxIns utxos tx = do
  runEitherT $ do
    let txOuts = Tx.txOutputs tx
        nonMintedValue = mconcat (map Tx.txOutValue txOuts) `minus` txMint tx
        minSpending =
          mconcat
            [ txFee tx
            , nonMintedValue
            ]
    txIns <- newEitherT $ selectTxIns @w (Set.fromList $ txInputs tx) utxos minSpending
    -- constantly adding inputs and running balance loop forever
    pure $
      tx
        { txInputs = Set.fromList (txInputs tx) ^.. to (<> txIns) . folded
        }

-- | Set collateral or fail in case it's required but not available
addTxCollaterals :: CollateralUtxo -> Tx -> Tx
addTxCollaterals cOut tx
  | txUsesScripts tx = tx {txCollateral = [Tx.pubKeyTxIn (collateralTxOutRef cOut)]}
  | otherwise = tx

txUsesScripts :: Tx -> Bool
txUsesScripts Tx {txInputs, txMintScripts} =
  not (null txMintScripts)
    || any
      (\TxIn {txInType} -> case txInType of Just ConsumeScriptAddress {} -> True; _ -> False)
      txInputs

-- | Ensures all non ada change goes back to user
handleNonAdaChange :: BalanceConfig -> Address -> Map TxOutRef TxOut -> Tx -> Either Text Tx
handleNonAdaChange balanceCfg changeAddr utxos tx =
  let nonAdaChange = getNonAdaChange utxos tx
      predicate =
        if bcSeparateChange balanceCfg
          then
            ( \txout ->
                Tx.txOutAddress txout == changeAddr
                  && not (justLovelace $ Tx.txOutValue txout)
            )
          else (\txout -> Tx.txOutAddress txout == changeAddr)
      newOutput =
        TxOut
          { txOutAddress = changeAddr
          , txOutValue = nonAdaChange
          , txOutDatumHash = Nothing
          }
      outputs =
        modifyFirst
          predicate
          (Just . maybe newOutput (addValueToTxOut nonAdaChange))
          (txOutputs tx)
   in if isValueNat nonAdaChange
        then Right $ if Value.isZero nonAdaChange then tx else tx {txOutputs = outputs}
        else Left "Not enough inputs to balance tokens."

{- | `addAdaChange` checks if `bcSeparateChange` is true,
      if it is then we add the ada change to seperate `TxOut` at changeAddr that contains only ada,
      else we add it to any `TxOut` present at changeAddr.
-}
addAdaChange :: BalanceConfig -> Address -> Integer -> Tx -> Tx
addAdaChange _ _ 0 tx = tx
addAdaChange balanceCfg changeAddr change tx
  | bcSeparateChange balanceCfg =
    tx
      { txOutputs =
          List.reverse $
            modifyFirst
              (\txout -> Tx.txOutAddress txout == changeAddr && justLovelace (txOutValue txout))
              (fmap $ addValueToTxOut $ Ada.lovelaceValueOf change)
              (List.reverse $ txOutputs tx)
      }
  | otherwise =
    tx
      { txOutputs =
          modifyFirst
            ((== changeAddr) . Tx.txOutAddress)
            (fmap $ addValueToTxOut $ Ada.lovelaceValueOf change)
            (txOutputs tx)
      }

addValueToTxOut :: Value -> TxOut -> TxOut
addValueToTxOut val txOut = txOut {txOutValue = txOutValue txOut <> val}

-- | Adds a 1 lovelace output to a transaction
addOutput :: Address -> Tx -> Tx
addOutput changeAddr tx = tx {txOutputs = txOutputs tx ++ [changeTxOut]}
  where
    changeTxOut =
      TxOut
        { txOutAddress = changeAddr
        , txOutValue = Ada.lovelaceValueOf 1
        , txOutDatumHash = Nothing
        }

{- | Add the required signatories to the transaction. Be aware the the signature itself is invalid,
 and will be ignored. Only the pub key hashes are used, mapped to signing key files on disk.
-}
addSignatories :: PubKeyHash -> Map PubKeyHash DummyPrivKey -> [PubKeyHash] -> Tx -> Either Text Tx
addSignatories ownPkh privKeys pkhs tx =
  foldM
    ( \tx' pkh ->
        case Map.lookup pkh privKeys of
          Just privKey -> Right $ Tx.addSignature' (unDummyPrivateKey privKey) tx'
          Nothing -> Left "Signing key not found."
    )
    tx
    (ownPkh : pkhs)

addValidRange ::
  forall (w :: Type) (effs :: [Type -> Type]).
  Member (PABEffect w) effs =>
  POSIXTimeRange ->
  Either CardanoBuildTx Tx ->
  Eff effs (Either Text Tx)
addValidRange _ (Left _) = pure $ Left "BPI is not using CardanoBuildTx"
addValidRange timeRange (Right tx) =
  if validateRange timeRange
    then
      bimap (Text.pack . show) (setRange tx)
        <$> posixTimeRangeToContainedSlotRange @w timeRange
    else pure $ Left "Invalid validity interval."
  where
    setRange tx' range = tx' {txValidRange = range}

validateRange :: forall (a :: Type). Ord a => Interval a -> Bool
validateRange (Interval (LowerBound PosInf _) _) = False
validateRange (Interval _ (UpperBound NegInf _)) = False
validateRange (Interval (LowerBound (Finite lowerBound) _) (UpperBound (Finite upperBound) _))
  | lowerBound >= upperBound = False
validateRange _ = True

{- | Modifies the first element matching a predicate, or, if none found, call the modifier with Nothing
 Calling this function ensures the modifier will always be run once
-}
modifyFirst ::
  forall (a :: Type).
  -- | Predicate for value to update
  (a -> Bool) ->
  -- | Modifier, input Maybe representing existing value (or Nothing if missing), output value representing new value (or Nothing to remove)
  (Maybe a -> Maybe a) ->
  [a] ->
  [a]
modifyFirst _ m [] = m Nothing `consJust` []
modifyFirst p m (x : xs) = if p x then m (Just x) `consJust` xs else x : modifyFirst p m xs

minus :: Value -> Value -> Value
minus x y =
  let negativeValues = map (\(c, t, a) -> (c, t, - a)) $ Value.flattenValue y
   in x <> mconcat (map unflattenValue negativeValues)

unflattenValue :: (CurrencySymbol, TokenName, Integer) -> Value
unflattenValue (curSymbol, tokenName, amount) =
  Value.assetClassValue (Value.assetClass curSymbol tokenName) amount

isValueNat :: Value -> Bool
isValueNat =
  all (\(_, _, a) -> a >= 0) . Value.flattenValue

justLovelace :: Value -> Bool
justLovelace value = length (Value.flattenValue value) == 1 && lovelaceValue value /= 0

consJust :: forall (a :: Type). Maybe a -> [a] -> [a]
consJust (Just x) = (x :)
consJust _ = id

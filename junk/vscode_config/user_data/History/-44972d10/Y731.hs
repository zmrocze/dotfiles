{-# OPTIONS_GHC -Wno-incomplete-uni-patterns -Wwarn #-}

module Test.CardanoWanchainBridge.Integration (test) where

import Cardano.Crypto.Wallet qualified as Wallet
import CardanoWanchainBridge.OffChain.AdaLocking qualified as AdaLocking
import CardanoWanchainBridge.OffChain.AggrPubKey qualified as AggrPubKey
import CardanoWanchainBridge.OffChain.MintToken qualified as MintToken
import CardanoWanchainBridge.OffChain.Types (
  BurnParams (BurnParams),
  LockParams (LockParams),
  MintParams (MintParams),
  UnlockParams (UnlockParams),
 )
import CardanoWanchainBridge.OnChain.AdaLocking (CrossChainTxMsg (CrossChainTxMsg), serialiseMsg)
import CardanoWanchainBridge.OnChain.AggrPubKey (
  AggrPubKey (AggrPubKey),
  AggrPubKeyDatum (AggrPubKeyDatum),
  AggrPubKeyParams (AggrPubKeyParams),
 )
import CardanoWanchainBridge.OnChain.MintToken qualified as MintToken
import Data.ByteString qualified as ByteString
import Ledger (pubKeyHash)
import Ledger qualified
import Ledger.Ada (lovelaceValueOf)
import Ledger.Address (PaymentPubKeyHash (PaymentPubKeyHash))
import Ledger.Crypto (PubKey)
import Ledger.Crypto qualified as Crypto
import Ledger.Tx (getCardanoTxId)
import Ledger.Value (unTokenName)
import MajorityMultiSign.Schema (MajorityMultiSignDatum (MajorityMultiSignDatum))
import Plutus.Contract (awaitTxConfirmed, ownPaymentPubKeyHash)
import Plutus.V1.Ledger.Value (assetClass, singleton)
import Test.Plutip.Contract (assertExecution, initAda, initLovelace, initLovelaceAssertValue, withContract, withContractAs)
import Test.Plutip.Internal.Types (ExecutionResult (outcome))
import Test.Plutip.LocalCluster (withCluster)
import Test.Plutip.Predicate (shouldFail, shouldSucceed)
import Test.Tasty (TestTree)
import Prelude

mockPrivKey :: Wallet.XPrv
mockPrivKey = Crypto.generateFromSeed' $ ByteString.replicate 32 121

mockPubKey :: PubKey
mockPubKey = Crypto.toPublicKey mockPrivKey

mockPrivKey2 :: Wallet.XPrv
mockPrivKey2 = Crypto.generateFromSeed' $ ByteString.replicate 32 122

mockPubKey2 :: PubKey
mockPubKey2 = Crypto.toPublicKey mockPrivKey2

mockPrivKey3 :: Wallet.XPrv
mockPrivKey3 = Crypto.generateFromSeed' $ ByteString.replicate 32 123

mockPubKey3 :: PubKey
mockPubKey3 = Crypto.toPublicKey mockPrivKey3

test :: TestTree
test =
  withCluster
    "Plutip integration test"
    [ assertExecution
        " wallet lock 80 ada, 2nd init pubkey, 3d unlock 30 ada, 2nd update pubkey, 4th unlock 20 ada with different privKey "
        ( initLovelaceAssertValue [200_000_000] (lovelaceValueOf 218_705_137)
            <> initLovelaceAssertValue [200_000_000] (lovelaceValueOf 119_846_400)
            <> initLovelaceAssertValue [200_000_000] (lovelaceValueOf 194_384_740)
            <> initLovelaceAssertValue [200_000_000] (lovelaceValueOf 228_705_137)
        )
        ( do
            aggParams <- withContractAs 1 $ \[pkh0, pkh2, pkh3] -> do
              let signers =
                    [pkh0, pkh2, pkh3]
                  initParams =
                    AggrPubKey.InitParams
                      (MajorityMultiSignDatum signers)
                      (AggrPubKeyDatum (AggrPubKey mockPubKey))
              AggrPubKey.init initParams

            _ <- withContract $
              const $ do
                lockTx <- AdaLocking.lock (LockParams 80_000_000 (extract aggParams).aggrAsset)
                awaitTxConfirmed (getCardanoTxId lockTx)
                return ()

            _ <- withContractAs 2 $
              const $ do
                oref <- AdaLocking.getInputUtxo
                PaymentPubKeyHash pkh <- ownPaymentPubKeyHash
                let amt = 30_000_000
                    aggrPubKeyParams = extract aggParams
                    msg =
                      serialiseMsg $
                        CrossChainTxMsg "ADA" amt pkh oref
                    sig = Crypto.sign' msg mockPrivKey
                unlockTx <- AdaLocking.unlock (UnlockParams oref sig amt aggrPubKeyParams)
                awaitTxConfirmed (getCardanoTxId unlockTx)

            _ <- withContractAs 1 $ \[pkh0, pkh2, pkh3] -> do
              let signers =
                    [pkh0, pkh2, pkh3]
                  aggrPubKeyParams = extract aggParams
              AggrPubKey.updateAggrPubKey aggrPubKeyParams (AggrPubKey mockPubKey2) signers

            withContractAs 3 $
              const $ do
                oref <- AdaLocking.getInputUtxo
                PaymentPubKeyHash pkh <- ownPaymentPubKeyHash
                let amt = 20_000_000
                    aggrPubKeyParams = extract aggParams
                    msg =
                      serialiseMsg $
                        CrossChainTxMsg "ADA" amt pkh oref
                    sig = Crypto.sign' msg mockPrivKey2
                unlockTx <- AdaLocking.unlock (UnlockParams oref sig amt aggrPubKeyParams)
                awaitTxConfirmed (getCardanoTxId unlockTx)
        )
        [shouldSucceed]
    , assertExecution
        " wallet lock 80 ada, 2nd init pubkey, 3d unlock 30 ada, 2nd update pubkey, 3d unlock 20 ada with the error"
        ( initLovelace [200_000_000]
            <> initLovelace [200_000_000]
            <> initLovelace [200_000_000]
        )
        ( do
            aggParams <- withContractAs 1 $ \[pkh0, pkh2] -> do
              let signers =
                    [pkh0, pkh2]
                  initParams =
                    AggrPubKey.InitParams
                      (MajorityMultiSignDatum signers)
                      (AggrPubKeyDatum (AggrPubKey mockPubKey))
              AggrPubKey.init initParams

            _ <- withContract $
              const $ do
                lockTx <- AdaLocking.lock (LockParams 80_000_000 (extract aggParams).aggrAsset)
                awaitTxConfirmed (getCardanoTxId lockTx)
                return ()

            _ <- withContractAs 2 $
              const $ do
                oref <- AdaLocking.getInputUtxo
                PaymentPubKeyHash pkh <- ownPaymentPubKeyHash
                let amt = 30_000_000
                    aggrPubKeyParams = extract aggParams
                    msg =
                      serialiseMsg $
                        CrossChainTxMsg "ADA" amt pkh oref
                    sig = Crypto.sign' msg mockPrivKey
                unlockTx <- AdaLocking.unlock (UnlockParams oref sig amt aggrPubKeyParams)
                awaitTxConfirmed (getCardanoTxId unlockTx)

            _ <- withContractAs 1 $ \[pkh0, pkh2] -> do
              let signers =
                    [pkh0, pkh2]
                  aggrPubKeyParams = extract aggParams
              AggrPubKey.updateAggrPubKey aggrPubKeyParams (AggrPubKey mockPubKey2) signers

            withContractAs 2 $
              const $ do
                oref <- AdaLocking.getInputUtxo
                PaymentPubKeyHash pkh <- ownPaymentPubKeyHash
                let amt = 20_000_000
                    aggrPubKeyParams = extract aggParams
                    msg =
                      serialiseMsg $
                        CrossChainTxMsg "ADA" amt pkh oref
                    sig = Crypto.sign' msg mockPrivKey
                unlockTx <- AdaLocking.unlock (UnlockParams oref sig amt aggrPubKeyParams)
                awaitTxConfirmed (getCardanoTxId unlockTx)
        )
        [shouldFail]
    , assertExecution
        " 1st wallet init pubkey, 2nd mint 5 tokens, 1st wallet update pubkey, 2nd mint 5 tokens with another privkey"
        ( initLovelaceAssertValue [200_000_000] ({-testTokenValue _2 "WAN" 10 <>-} lovelaceValueOf 197_756_870)
            <> initLovelaceAssertValue [200_000_000] (lovelaceValueOf 194_441_850)
        )
        ( do
            aggParams <- withContractAs 0 $ \[pkh0] -> do
              pkh <- ownPaymentPubKeyHash
              let signers = [pkh0, pkh]
                  initParams =
                    AggrPubKey.InitParams
                      (MajorityMultiSignDatum signers)
                      (AggrPubKeyDatum (AggrPubKey mockPubKey))
              AggrPubKey.init initParams

            _ <- withContractAs 1 $
              const $ do
                oref <- AdaLocking.getInputUtxo
                PaymentPubKeyHash pkh <- ownPaymentPubKeyHash
                let mintAmt = 5
                    tokenName = "WAN"
                    aggrPubKeyParams = extract aggParams
                    msg =
                      serialiseMsg $
                        CrossChainTxMsg (unTokenName tokenName) mintAmt pkh oref
                    sig = Crypto.sign' msg mockPrivKey
                MintToken.mint (MintParams oref sig mintAmt tokenName aggrPubKeyParams)

            _ <- withContractAs 0 $ \[pkh0] -> do
              pkh <- ownPaymentPubKeyHash
              let signers =
                    [pkh0, pkh]
                  aggrPubKeyParams = extract aggParams
              AggrPubKey.updateAggrPubKey aggrPubKeyParams (AggrPubKey mockPubKey2) signers

            withContractAs 1 $
              const $ do
                oref <- AdaLocking.getInputUtxo
                PaymentPubKeyHash pkh <- ownPaymentPubKeyHash
                let mintAmt = 5
                    tokenName = "WAN"
                    aggrPubKeyParams = extract aggParams
                    msg =
                      serialiseMsg $
                        CrossChainTxMsg (unTokenName tokenName) mintAmt pkh oref
                    sig = Crypto.sign' msg mockPrivKey2
                MintToken.mint (MintParams oref sig mintAmt tokenName aggrPubKeyParams)
        )
        [shouldSucceed]
    , assertExecution
        " 1 st wallet init pubkey, 2nd mint 5 tokens, 1st wallet update pubkey, 2nd mint 5 tokens with same privkey - should fail"
        (initLovelace [200_000_000] <> initLovelace [200_000_000])
        ( do
            aggParams <- withContractAs 0 $ \[pkh0] -> do
              pkh <- ownPaymentPubKeyHash
              let signers = [pkh0, pkh]
                  initParams =
                    AggrPubKey.InitParams
                      (MajorityMultiSignDatum signers)
                      (AggrPubKeyDatum (AggrPubKey mockPubKey))
              AggrPubKey.init initParams

            _ <- withContractAs 1 $
              const $ do
                oref <- AdaLocking.getInputUtxo
                PaymentPubKeyHash pkh <- ownPaymentPubKeyHash
                let mintAmt = 5
                    tokenName = "WAN"
                    aggrPubKeyParams = extract aggParams
                    msg =
                      serialiseMsg $
                        CrossChainTxMsg (unTokenName tokenName) mintAmt pkh oref
                    sig = Crypto.sign' msg mockPrivKey
                MintToken.mint (MintParams oref sig mintAmt tokenName aggrPubKeyParams)

            _ <- withContractAs 0 $ \[pkh0] -> do
              pkh <- ownPaymentPubKeyHash
              let signers =
                    [pkh0, pkh]
                  aggrPubKeyParams = extract aggParams
              AggrPubKey.updateAggrPubKey aggrPubKeyParams (AggrPubKey mockPubKey2) signers

            withContractAs 1 $
              const $ do
                oref <- AdaLocking.getInputUtxo
                PaymentPubKeyHash pkh <- ownPaymentPubKeyHash
                let mintAmt = 5
                    tokenName = "WAN"
                    aggrPubKeyParams = extract aggParams
                    msg =
                      serialiseMsg $
                        CrossChainTxMsg (unTokenName tokenName) mintAmt pkh oref
                    sig = Crypto.sign' msg mockPrivKey
                MintToken.mint (MintParams oref sig mintAmt tokenName aggrPubKeyParams)
        )
        [shouldFail]
    , -- this test works properly solely, but in the big tree in plutip it drops error, because all tests use same plutip cluster
      --  , assertExecution
      --     " AdaLocking.unlock"
      --     (initAda [100,100])
      --     ( withContract $
      --         const
      --           ( do
      --               let amt = 2_345_678
      --                   initParams =
      --                     AggrPubKey.InitParams
      --                       ( MajorityMultiSignDatum
      --                           [ PaymentPubKeyHash $ pubKeyHash mockPubKey2
      --                           , PaymentPubKeyHash $ pubKeyHash mockPubKey3
      --                           ]
      --                       )
      --                       (AggrPubKeyDatum (AggrPubKey mockPubKey))

      --               lockTx <- AdaLocking.lock (LockParams amt)
      --               awaitTxConfirmed (getCardanoTxId lockTx)

      --               aggrPubKeyParams <- AggrPubKey.init initParams

      --               oref <- AdaLocking.getInputUtxo
      --               PaymentPubKeyHash pkh <- ownPaymentPubKeyHash
      --               let msg =
      --                     serialiseMsg $
      --                       CrossChainTxMsg "ADA" amt pkh oref
      --                   sig = Crypto.sign' msg mockPrivKey

      --               AdaLocking.unlock (UnlockParams oref sig amt aggrPubKeyParams)
      --           )
      --     )
      --     [shouldSucceed]
      assertExecution
        " MintToken.mint"
        (initAda [100])
        ( withContract $
            const
              ( do
                  let initParams =
                        AggrPubKey.InitParams
                          ( MajorityMultiSignDatum
                              [ PaymentPubKeyHash $ pubKeyHash mockPubKey2
                              , PaymentPubKeyHash $ pubKeyHash mockPubKey3
                              ]
                          )
                          (AggrPubKeyDatum (AggrPubKey mockPubKey))

                  aggrPubKeyParams <- AggrPubKey.init initParams

                  oref <- AdaLocking.getInputUtxo
                  PaymentPubKeyHash pkh <- ownPaymentPubKeyHash
                  let tokenAmt = 1
                      tokenName = "WAN"
                      msg =
                        serialiseMsg $
                          CrossChainTxMsg (unTokenName tokenName) tokenAmt pkh oref
                      sig = Crypto.sign' msg mockPrivKey

                  MintToken.mint (MintParams oref sig tokenAmt tokenName aggrPubKeyParams)
              )
        )
        [shouldSucceed]
    , assertExecution
        " MintToken.burn"
        (initAda [100])
        ( withContract $
            const
              ( do
                  let initParams =
                        AggrPubKey.InitParams
                          ( MajorityMultiSignDatum
                              [ PaymentPubKeyHash $ pubKeyHash mockPubKey2
                              , PaymentPubKeyHash $ pubKeyHash mockPubKey3
                              ]
                          )
                          (AggrPubKeyDatum (AggrPubKey mockPubKey))

                  aggrPubKeyParams <- AggrPubKey.init initParams

                  oref <- AdaLocking.getInputUtxo
                  PaymentPubKeyHash pkh <- ownPaymentPubKeyHash
                  let tokenAmt = 2
                      tokenName = "WAN"
                      msg =
                        serialiseMsg $
                          CrossChainTxMsg (unTokenName tokenName) tokenAmt pkh oref
                      sig = Crypto.sign' msg mockPrivKey

                  MintToken.mint (MintParams oref sig tokenAmt tokenName aggrPubKeyParams)
                  MintToken.burn $ BurnParams (negate tokenAmt) tokenName aggrPubKeyParams.aggrAsset
              )
        )
        [shouldSucceed]
    , -- this test works properly solely, but in the big tree in plutip it drops error, because all tests use same plutip cluster
      -- , assertExecution
      --     " AdaLocking 2 wallets lock and unlock ada partially"
      --     (initAndAssertLovelace [100_000_000 , 100_000_000] 134_477_906 <> initAndAssertLovelace [110_000_000] 119_846_400)
      --     ( do
      --         let amtLock = 80_000_000
      --             amtUnlock = 30_000_000
      --             signers =
      --               [ PaymentPubKeyHash $ pubKeyHash mockPubKey2
      --               , PaymentPubKeyHash $ pubKeyHash mockPubKey3
      --               ]
      --             initParams =
      --               AggrPubKey.InitParams
      --                 ( MajorityMultiSignDatum
      --                     signers
      --                 )
      --                 (AggrPubKeyDatum (AggrPubKey mockPubKey))
      --         _ <- withContract $
      --           const $ do
      --             lockTx <- AdaLocking.lock (LockParams amtLock)
      --             awaitTxConfirmed (getCardanoTxId lockTx)
      --         withContractAs 1 $
      --           const $ do
      --             aggrPubKeyParams <- AggrPubKey.init initParams
      --             oref <- AdaLocking.getInputUtxo
      --             PaymentPubKeyHash pkh' <- ownPaymentPubKeyHash
      --             let msg =
      --                   serialiseMsg $
      --                     CrossChainTxMsg "ADA" amtUnlock pkh' oref
      --                 sig = Crypto.sign' msg mockPrivKey

      --             unlockTx <- AdaLocking.unlock (UnlockParams oref sig amtUnlock aggrPubKeyParams)
      --             awaitTxConfirmed (getCardanoTxId unlockTx)
      --     )
      --     [shouldSucceed]
      assertExecution
        " 1 wallet mint 10 tokens and burn 5"
        (initLovelaceAssertValue [200_000_000] ({-testTokenValue _1 "WAN" 5 <>-} lovelaceValueOf 194_054_476))
        ( withContract $
            const
              ( do
                  let initParams =
                        AggrPubKey.InitParams
                          ( MajorityMultiSignDatum
                              [ PaymentPubKeyHash $ pubKeyHash mockPubKey2
                              , PaymentPubKeyHash $ pubKeyHash mockPubKey3
                              ]
                          )
                          (AggrPubKeyDatum (AggrPubKey mockPubKey))

                  aggrPubKeyParams <- AggrPubKey.init initParams

                  oref <- AdaLocking.getInputUtxo
                  PaymentPubKeyHash pkh <- ownPaymentPubKeyHash
                  let mintAmt = 10
                      burnAmt = (-5)
                      tokenName = "WAN"
                      msg =
                        serialiseMsg $
                          CrossChainTxMsg (unTokenName tokenName) mintAmt pkh oref
                      sig = Crypto.sign' msg mockPrivKey

                  MintToken.mint (MintParams oref sig mintAmt tokenName aggrPubKeyParams)
                  MintToken.burn $ BurnParams burnAmt tokenName aggrPubKeyParams.aggrAsset
              )
        )
        [shouldSucceed]
    , assertExecutionWith
        " update public key"
        ( initLovelaceAssertValue [200_000_000] (lovelaceValueOf 194_430_011)
            <> initLovelaceAssertValue [200_000_000] (lovelaceValueOf 200_000_000)
            <> initLovelaceAssertValue [200_000_000] (lovelaceValueOf 200_000_000)
        )
        ( withContract $ \[pkh1, pkh2] -> do
            let signers =
                  [pkh1, pkh2]
                initParams =
                  AggrPubKey.InitParams
                    (MajorityMultiSignDatum signers)
                    (AggrPubKeyDatum (AggrPubKey mockPubKey))
            aggrPubKeyParams <- AggrPubKey.init initParams
            AggrPubKey.updateAggrPubKey aggrPubKeyParams (AggrPubKey mockPubKey) signers
        )
        [shouldSucceed]
    ]

testTokenValue :: MintToken.MintTokenParams -> Ledger.TokenName -> Integer -> Ledger.Value
testTokenValue = singleton . mintCurrencySymbol

mintCurrencySymbol :: MintToken.MintTokenParams -> Ledger.CurrencySymbol
mintCurrencySymbol = Ledger.scriptCurrencySymbol . MintToken.policy

extract :: ExecutionResult w e (AggrPubKeyParams, b) -> AggrPubKeyParams
extract aggParams = case outcome aggParams of
  Right x -> fst x
  Left _ -> let assetNone = assetClass " " " " in AggrPubKeyParams assetNone assetNone

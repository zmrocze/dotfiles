{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE FlexibleContexts #-}

module Main (main) where

import Cardano.Launcher.Node (CardanoNodeConn, nodeSocketFile)
import Cardano.Ledger.Slot (EpochSize (EpochSize))
import Control.Applicative (optional, (<**>))
import Control.Monad (forM_, void)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, ToJSON, encodeFile)
import Data.Default (def)
import Data.Time (NominalDiffTime)
import GHC.Conc (TVar, threadDelay)
import GHC.Generics (Generic)
import GHC.Natural (Natural)
import GHC.Word (Word64)
import Numeric.Positive (Positive)
import Options.Applicative (Parser, helper, info)
import Options.Applicative qualified as Options
import System.Posix (Handler (CatchOnce), installHandler, sigINT)
import Test.Plutip.Config (
  PlutipConfig (clusterWorkingDir, extraConfig),
  WorkingDirectory (Fixed, Temporary),
 )
import Test.Plutip.Internal.Cluster.Extra.Types (
  ExtraConfig (ExtraConfig),
 )
import Test.Plutip.Internal.LocalCluster (
  ClusterStatus,
  stopCluster, startFundedCluster, dieOnError
 )
import Test.Plutip.Internal.Types (nodeSocket)
import Test.Plutip.Internal.Keys (showPkh, KeyPair, mainnetAddress, saveKeyPair)
import Data.Foldable (for_)
import Test.Plutip.Internal.DistributeFunds (Lovelace)

main :: IO ()
main = do
  config <- Options.execParser (info (pClusterConfig <**> helper) mempty)
  case totalAmount config of
    Left e -> error e
    Right amt -> do
      let ClusterConfig {numWallets, dirWallets, numUtxos, workDir, slotLength, epochSize} = config
          workingDir = maybe Temporary (`Fixed` False) workDir

          extraConf = ExtraConfig slotLength epochSize
          plutipConfig = def {clusterWorkingDir = workingDir, extraConfig = extraConf}

      putStrLn "Starting cluster..."
      (st, _) <- startFundedCluster plutipConfig (replicate numWallets $ replicate numUtxos amt) $ \cenv keys -> do
        -- Save keys to requested directory 
        forM_ dirWallets $ \dir -> for_ keys (fmap dieOnError . saveKeyPair dir)

        -- print info
        separate
        liftIO $ forM_ (zip keys [(1 :: Int) ..]) printWallet
        printNodeRelatedInfo cenv
        separate

        -- Dump cluster info to local-cluster.info
        forM_ (dumpInfo config) $ \dInfo -> do
          dumpClusterInfo
            dInfo
            (nodeSocket cenv)
            keys

      void $ installHandler sigINT (termHandler st) Nothing
      putStrLn "Cluster is running. Ctrl-C to stop."
      loopThreadDelay
  where
    loopThreadDelay = threadDelay 100000000 >> loopThreadDelay

    printNodeRelatedInfo = \cEnv -> do
      putStrLn $ "Node socket: " <> show (nodeSocket cEnv)

    separate = liftIO $ putStrLn "\n------------\n"

    totalAmount :: ClusterConfig -> Either String Lovelace
    totalAmount cwc =
      case toAda (adaAmount cwc) + lvlAmount cwc of
        0 -> Left "One of --ada or --lovelace arguments should not be 0"
        amt -> Right $ fromInteger . toInteger $ amt

    dumpClusterInfo :: FilePath -> CardanoNodeConn -> [KeyPair] -> IO ()
    dumpClusterInfo fp nodeConn ws = do
      encodeFile
        fp
        ( ClusterInfo
            { ciWallets = [(showPkh w, show . mainnetAddress $ w) | w <- ws]
            , ciNodeSocket = nodeSocketFile nodeConn
            }
        )

    printWallet (w, n) = do
      putStrLn $ "Wallet " ++ show n ++ " PKH: " ++ showPkh w
      putStrLn $ "Wallet " ++ show n ++ " mainnet address: " ++ mainnetAddress w

    toAda = (* 1_000_000)

termHandler :: TVar (ClusterStatus ()) -> System.Posix.Handler
termHandler st = CatchOnce $ do
  putStrLn "Caught SIGTERM, stopping cluster"
  stopCluster st

data ClusterInfo = ClusterInfo
  { ciWallets :: [(String, String)]
  , ciNodeSocket :: String
  }
  deriving (Show, Generic, ToJSON, FromJSON)

pnumWallets :: Parser Int
pnumWallets =
  Options.option
    Options.auto
    ( Options.long "num-wallets"
        <> Options.long "wallets"
        <> Options.short 'n'
        <> Options.metavar "NUM_WALLETS"
        <> Options.value 1
    )

pdirWallets :: Parser (Maybe FilePath)
pdirWallets =
  optional $
    Options.strOption
      ( Options.long "wallets-dir"
          <> Options.long "wallet-dir"
          <> Options.short 'd'
          <> Options.metavar "FILEPATH"
      )

padaAmount :: Parser Natural
padaAmount =
  Options.option
    Options.auto
    ( Options.long "ada"
        <> Options.short 'a'
        <> Options.metavar "ADA"
        <> Options.value 10_000
    )

plvlAmount :: Parser Natural
plvlAmount =
  Options.option
    Options.auto
    ( Options.long "lovelace"
        <> Options.short 'l'
        <> Options.metavar "Lovelace"
        <> Options.value 0
    )

pnumUtxos :: Parser Int
pnumUtxos =
  Options.option
    Options.auto
    ( Options.long "utxos"
        <> Options.short 'u'
        <> Options.metavar "NUM_UTXOS"
        <> Options.value 1
    )

pWorkDir :: Parser (Maybe FilePath)
pWorkDir =
  optional $
    Options.strOption
      ( Options.long "working-dir"
          <> Options.short 'w'
          <> Options.metavar "FILEPATH"
      )

pSlotLen :: Parser NominalDiffTime
pSlotLen =
  Options.option
    Options.auto
    ( Options.long "slot-len"
        <> Options.short 's'
        <> Options.metavar "SLOT_LEN"
        <> Options.value 0.2
    )

pEpochSize :: Parser EpochSize
pEpochSize =
  EpochSize <$> wordParser
  where
    wordParser :: Parser Word64
    wordParser =
      Options.option
        Options.auto
        ( Options.long "epoch-size"
            <> Options.short 'e'
            <> Options.metavar "EPOCH_SIZE"
            <> Options.value 160
        )

pInfoJson :: Parser (Maybe FilePath)
pInfoJson =
  optional $
    Options.strOption
      ( Options.long "dump-info-json"
          <> Options.metavar "FILEPATH"
          <> Options.help "After starting the cluster, add some useful runtime information to a JSON file (wallets, node socket path etc)"
          <> Options.value "local-cluster-info.json"
      )

pClusterConfig :: Parser ClusterConfig
pClusterConfig =
  ClusterConfig
    <$> pnumWallets
    <*> pdirWallets
    <*> padaAmount
    <*> plvlAmount
    <*> pnumUtxos
    <*> pWorkDir
    <*> pSlotLen
    <*> pEpochSize
    <*> pInfoJson

-- | Basic info about the cluster, to
-- be used by the command-line
data ClusterConfig = ClusterConfig
  { numWallets :: Int
  , dirWallets :: Maybe FilePath
  , adaAmount :: Natural
  , lvlAmount :: Natural
  , numUtxos :: Int
  , workDir :: Maybe FilePath
  , slotLength :: NominalDiffTime
  , epochSize :: EpochSize
  , dumpInfo :: Maybe FilePath
  }
  deriving stock (Show, Eq)

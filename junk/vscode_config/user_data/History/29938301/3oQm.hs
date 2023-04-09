
module Test.Plutip.Internal.DistributeFunds where

-- module Test.Plutip.Internal.BotPlutusInterface.Wallet (
--   BpiWallet (..),
--   addSomeWallet,
--   addSomeWalletDir,
--   eitherAddSomeWallet,
--   eitherAddSomeWalletDir,
--   mkMainnetAddress,
--   cardanoMainnetAddress,
--   ledgerPaymentPkh,
--   showAddress,
-- ) where

import Cardano.Api (AddressAny, PaymentKey, SigningKey, VerificationKey)
import Cardano.Api qualified as CAPI
import Cardano.BM.Data.Tracer (nullTracer)
import Cardano.Wallet.Primitive.Types.Coin (Coin (Coin))

-- import Cardano.Wallet.Shelley.Launch.Cluster (
--   sendFaucetFundsTo,
--  )

import Control.Arrow (ArrowChoice (left))
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT, ask)
-- import Data.Aeson.Extras (encodeByteString)
import Data.Bool (bool)
import Data.Text qualified as Text
import Numeric.Positive (Positive)
import Plutus.V1.Ledger.Api qualified as LAPI
import PlutusTx.Builtins (fromBuiltin, toBuiltin)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((<.>), (</>))
import Test.Plutip.Internal.BotPlutusInterface.Setup qualified as Setup
import Test.Plutip.Internal.BotPlutusInterface.Types (BpiError (BotInterfaceDirMissing, SignKeySaveError))
import Test.Plutip.Internal.Types (ClusterEnv, nodeSocket, supportDir)
import Plutus.V1.Ledger.Api (PubKeyHash (PubKeyHash))

import Data.ByteString qualified as B
import Data.ByteString.Base16 qualified as Base16
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Test.Plutip.Internal.Keys (KeyPair)

import Cardano.Address.Derivation
    ( XPub, xpubPublicKey )

import Cardano.Api
    ( AsType (AsStakeKey, AsStakePoolKey)
    , Key (verificationKeyHash)
    , serialiseToCBOR
    )
-- import Cardano.Wallet.Api.Types
--     ( decodeAddress )
import Cardano.Api.Shelley
    ( AsType (AsVrfKey) )
import Cardano.Binary
    ( fromCBOR )

import System.IO.Temp
    ( createTempDirectory, getCanonicalTemporaryDirectory )

import UnliftIO.Temporary
    ( withTempDirectory )

import Cardano.BM.Data.Output
    ( ScribeDefinition (ScribeDefinition, scName, scFormat, scKind, scMinSev, scMaxSev, scPrivacy, scRotation)
    , ScribeFormat (ScText)
    , ScribeKind (FileSK, StdoutSK, StderrSK)
    , ScribePrivacy (ScPublic), ScribeId
    )
import Cardano.BM.Data.Severity
    ( Severity (Critical, Debug, Notice, Info, Error, Warning) )
import Cardano.BM.Data.Tracer
    ( HasPrivacyAnnotation, HasSeverityAnnotation (getSeverityAnnotation) )
-- import Cardano.Api
--     ( parseLoggingSeverity )
import Control.Monad.IO.Unlift
    ( MonadUnliftIO, liftIO )
-- import Cardano.CLI ( parseLoggingSeverity )
import Cardano.CLI.Byron.Commands
    ( VerificationKeyFile (VerificationKeyFile) )
import Cardano.CLI.Shelley.Key
    ( VerificationKeyOrFile (..), readVerificationKeyOrFile )
import Cardano.Launcher
    ( LauncherLog, ProcessHasExited (..) )
import Cardano.Launcher.Node
    ( CardanoNodeConfig (..)
    , CardanoNodeConn
    , NodePort (..)
    , nodeSocketFile
    , withCardanoNode
    )
import Cardano.Ledger.BaseTypes
    ( Network (Mainnet)
    , NonNegativeInterval
    , PositiveUnitInterval
    , StrictMaybe (..)
    , UnitInterval
    , boundRational
    , textToUrl
    )
import Cardano.Ledger.Crypto
    ( StandardCrypto )
import Cardano.Ledger.Era
    ( Era (Crypto) )
import Cardano.Ledger.Shelley.API
    ( ShelleyGenesis (sgInitialFunds, sgStaking, ShelleyGenesis, sgProtocolParams, sgNetworkMagic), ShelleyGenesisStaking (sgsPools) )
import Cardano.Pool.Metadata
    ( SMASHPoolId (..), HealthStatusSMASH(HealthStatusSMASH) )
import Cardano.Startup
    ( restrictFileMode )
import UnliftIO.Exception
    ( bracket )
import Cardano.BM.Setup ( setupTrace_, shutdown )
-- import Cardano.Wallet.Api.Http.Server
--     ( Listen (..) )
-- import Cardano.Wallet.Api
--     ( ApiEra (..) )
-- import Cardano.Wallet.Api.Types
--     ( ApiEra (..) 
    -- , DecodeAddress (..), HealthStatusSMASH (..) )
import Cardano.Wallet.Logging
    ( BracketLog, bracketTracer, BracketLog'(BracketStart) )
import Cardano.Wallet.Network.Ports
    ( randomUnusedTCPPorts )
import Cardano.Wallet.Primitive.AddressDerivation
    ( hex )
import Cardano.Wallet.Primitive.Types
    ( Block
    , EpochNo
    , NetworkParameters
    , PoolCertificate, unEpochNo
    -- , PoolId (..)
    )
import Cardano.Wallet.Primitive.Types.Address
    ( Address (Address) )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (Coin, unCoin) )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( AssetId (AssetId), TokenBundle (TokenBundle) )
import Cardano.Wallet.Primitive.Types.TokenPolicy
    ( TokenName (UnsafeTokenName) )
import Cardano.Wallet.Primitive.Types.TokenQuantity
    ( TokenQuantity (TokenQuantity) )
import Cardano.Wallet.Shelley.Compatibility
    ( StandardShelley, fromGenesisData )
-- import Cardano.Wallet.Shelley.Launch
--     ( TempDirLog, envFromText, lookupEnvNonEmpty )
import Cardano.Wallet.Unsafe
    ( unsafeBech32Decode, unsafeFromHex )
import Cardano.Wallet.Util
    ( mapFirst )
import Codec.Binary.Bech32.TH
    ( humanReadablePart )
import Control.Monad
    ( forM, forM_, liftM2, replicateM, replicateM_, void, when, (>=>) )
import Control.Retry
    ( constantDelay, limitRetriesByCumulativeDelay, retrying )
import Control.Tracer
    ( Tracer (..), contramap, traceWith )
import Crypto.Hash.Utils
    ( blake2b256 )
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Data.Aeson
    ( object, toJSON, (.:), (.=), ToJSON, Options (fieldLabelModifier, omitNothingFields), genericToJSON, camelTo2 )
import Data.Aeson.QQ
    ( aesonQQ )
import Data.Bits
    ( (.|.) )
import Data.ByteArray.Encoding
    ( Base (..), convertToBase )
import Data.ByteString
    ( ByteString )
import Data.ByteString.Base58
    ( bitcoinAlphabet, decodeBase58 )
import Data.Char
    ( toLower )
import Data.Either
    ( fromRight, isLeft, isRight )
import Data.Foldable
    ( traverse_ )
import Data.Generics.Product.Fields
    ( setField )
import Data.IntCast
    ( intCast )
import Data.List
    ( intercalate, nub, permutations, sort )
import Data.Map
    ( Map )
import Data.Maybe
    ( catMaybes, fromMaybe, isJust )
import Data.Text
    ( Text )
import Data.Text.Class
    ( ToText(toText) )
import Data.Time.Clock
    ( UTCTime, addUTCTime, getCurrentTime )
import Data.Time.Clock.POSIX
    ( posixSecondsToUTCTime, utcTimeToPOSIXSeconds )
import Ouroboros.Network.Magic
    ( NetworkMagic (..) )
import Ouroboros.Network.NodeToClient
    ( NodeToClientVersionData (..) )
import System.Directory
    ( copyFile, createDirectory, createDirectoryIfMissing, makeAbsolute )
import System.Environment
    ( getEnvironment, lookupEnv )
import System.Exit
    ( ExitCode (..), die )
import System.FilePath
    ( (<.>), (</>) )
import System.IO.Unsafe
    ( unsafePerformIO )
import System.Process.Typed
    ( ProcessConfig, proc, readProcess, setEnv, setEnvInherit )
import Test.Utils.Paths
    ( getTestData )
import Test.Utils.StaticServer
    ( withStaticServer )
import UnliftIO.Async
    ( async, link, wait )
import UnliftIO.Chan
    ( newChan, readChan, writeChan )
import UnliftIO.Exception
    ( SomeException, finally, handle, throwIO, throwString )
import UnliftIO.MVar
    ( MVar, modifyMVar, newMVar, swapMVar )

import qualified Cardano.Ledger.Address as Ledger
import qualified Cardano.Ledger.Shelley.API as Ledger
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Cardano.Wallet.Primitive.Types.TokenMap as TokenMap
import Cardano.Pool.Types
    ( PoolId (PoolId) )
import qualified Codec.Binary.Bech32 as Bech32
import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Read as CBOR
import qualified Codec.CBOR.Write as CBOR
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Aeson
import qualified Data.Aeson.KeyMap as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import qualified Data.Yaml as Yaml

import Data.Default (def)
import Test.Plutip.Internal.Cluster.Extra.Types (ExtraConfig, ecSlotLength, ecEpochSize)
import Test.Plutip.Internal.Cluster.FaucetFunds (faucetFunds)
import GHC.TypeLits (Symbol)
import Cardano.BM.Data.LogItem (LoggerName)
import Cardano.BM.Backend.Switchboard (Switchboard)
import qualified Cardano.BM.Configuration.Model as CM
import Cardano.BM.Data.Trace (Trace)
import qualified Cardano.BM.Data.Backend as CM
import Cardano.BM.Configuration.Static (defaultConfigStdout)
import Cardano.BM.Trace (logDebug, appendName)
import Test.Plutip.Internal.Cluster.PoolConfigs (PoolRecipe (PoolRecipe, operatorKeys), defaultPoolConfigs)
import Test.Plutip.Internal.Cluster (ClusterLog)

fundKey :: ClusterEnv -> KeyPair -> [Positive] -> IO ()
fundKey cenv keys funds = do
  let fundAddress = mainnetAddress keys
      toAmt = Coin . fromIntegral
  sendFaucetFundsTo
    nullTracer -- todo: fix tracer to be not `nullTracer`
    (nodeSocket cEnv)
    (supportDir cEnv)
    [(fundAddress, toAmt v) | v <- funds]

sendFaucetFundsTo
    :: Tracer IO ClusterLog
    -> CardanoNodeConn
    -> FilePath
    -> [(String, Coin)]
    -> IO ()
sendFaucetFundsTo tr conn dir targets = batch 80 targets $
    sendFaucet tr conn dir "ada" . map coinBundle
  where
    coinBundle = fmap (\c -> (TokenBundle.fromCoin c, []))

-- | Create transactions to fund the given faucet addresses with Ada and assets.
--
-- Beside the 'TokenBundle' of Ada and assets, there is a list of
-- @(signing key, verification key hash)@ pairs needed to sign the
-- minting transaction.
sendFaucetAssetsTo
    :: Tracer IO ClusterLog
    -> CardanoNodeConn
    -> FilePath
    -> Int -- ^ batch size
    -> [(String, (TokenBundle, [(String, String)]))] -- ^ (address, assets)
    -> IO ()
sendFaucetAssetsTo tr conn dir batchSize targets = do
    era <- getClusterEra dir
    when (era >= MaryHardFork) $
        batch batchSize targets $ sendFaucet tr conn dir "assets"

-- | Build, sign, and send a batch of faucet funding transactions using
-- @cardano-cli@. This function is used by 'sendFaucetFundsTo' and
-- 'sendFaucetAssetsTo'.
sendFaucet
    :: Tracer IO ClusterLog
    -> CardanoNodeConn
    -> FilePath
    -> String -- ^ label for logging
    -> [(String, (TokenBundle, [(String, String)]))]
    -> IO ()
sendFaucet tr conn dir what targets = do
    (faucetInput, faucetPrv) <- takeFaucet
    let file = dir </> "faucet-tx.raw"

    let mkOutput addr (TokenBundle (Coin c) tokens) =
            [ "--tx-out"
            , unwords $ [ addr, show c, "lovelace"] ++
                map (("+ " ++) . cliAsset) (TokenMap.toFlatList tokens)
            ]
        cliAsset (aid, (TokenQuantity q)) = unwords [show q, cliAssetId aid]
        cliAssetId (AssetId pid (UnsafeTokenName name)) = mconcat
            [ T.unpack (toText pid)
            , if B8.null name then "" else "."
            , B8.unpack (hex name)
            ]
        mkMint [] = []
        mkMint assets = ["--mint", intercalate " + " (map cliAsset assets)]

    let total = fromIntegral $ sum $
            map (unCoin . TokenBundle.getCoin . fst . snd) targets
    when (total > faucetAmt) $ error "sendFaucetFundsTo: too much to pay"

    let targetAssets = concatMap (snd . TokenBundle.toFlatList . fst . snd) targets

    scripts <- forM (nub $ concatMap (map snd . snd . snd) targets) $
        writeMonetaryPolicyScriptFile dir

    cli tr $
        [ "transaction", "build-raw"
        , "--tx-in", faucetInput
        , "--ttl", "6000000"
            -- Big enough to allow minting in the actual integration tests,
            -- before the wallet API supports it.
        , "--fee", show (faucetAmt - total)
        , "--out-file", file
        ] ++
        concatMap (uncurry mkOutput . fmap fst) targets ++
        mkMint targetAssets ++
        (concatMap (\f -> ["--minting-script-file", f]) scripts)

    policyKeys <- forM (nub $ concatMap (snd . snd) targets) $
        \(skey, keyHash) -> writePolicySigningKey dir keyHash skey

    tx <- signTx tr dir file (faucetPrv:policyKeys)
    submitTx tr conn (what ++ " faucet tx") tx

batch :: Int -> [a] -> ([a] -> IO b) -> IO ()
batch s xs = forM_ (group s xs)
  where
    -- TODO: Use split package?
    -- https://stackoverflow.com/questions/12876384/grouping-a-list-into-lists-of-n-elements-in-haskell
    group :: Int -> [a] -> [[a]]
    group _ [] = []
    group n l
      | n > 0 = (take n l) : (group n (drop n l))
      | otherwise = error "Negative or zero n"

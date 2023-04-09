
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
import Test.Plutip.Internal.Cluster (
  sendFaucetFundsTo,
 )

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

fundKey :: ClusterEnv -> KeyPair -> [Positive] -> IO ()
fundKey cenv keys funds = do
  let fundAddress = mainnetAddress keys
      toAmt = Coin . fromIntegral
  sendFaucetFundsTo
    nullTracer -- todo: fix tracer to be not `nullTracer`
    (nodeSocket cEnv)
    (supportDir cEnv)
    [(fundAddress, toAmt v) | v <- funds]


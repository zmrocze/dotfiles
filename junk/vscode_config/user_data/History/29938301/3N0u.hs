
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

import Cardano.BM.Data.Tracer (nullTracer)
import Cardano.Wallet.Primitive.Types.Coin (Coin (Coin))

-- import Cardano.Wallet.Shelley.Launch.Cluster (
--   sendFaucetFundsTo,
--  )
import Test.Plutip.Internal.Cluster (
  sendFaucetFundsTo,
 )

-- import Data.Aeson.Extras (encodeByteString)
import Numeric.Positive (Positive)
import Test.Plutip.Internal.Types (ClusterEnv, nodeSocket, supportDir)

import Test.Plutip.Internal.Keys (KeyPair, mainnetAddress)

fundKey :: ClusterEnv -> KeyPair -> [Positive] -> IO ()
fundKey cEnv keys funds = do
  let fundAddress = mainnetAddress keys
      toAmt = Coin . fromIntegral
  sendFaucetFundsTo
    nullTracer -- todo: fix tracer to be not `nullTracer`
    (nodeSocket cEnv)
    (supportDir cEnv)
    [(fundAddress, toAmt v) | v <- funds]


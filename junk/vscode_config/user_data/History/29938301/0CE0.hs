
module Test.Plutip.Internal.DistributeFunds where

import Cardano.BM.Data.Tracer (nullTracer)
import Cardano.Wallet.Primitive.Types.Coin (Coin (Coin))

import Test.Plutip.Internal.Cluster (
  sendFaucetFundsTo,
 )

import Test.Plutip.Internal.Types (ClusterEnv, nodeSocket, supportDir)

import Test.Plutip.Internal.Keys (KeyPair, mainnetAddress)
import Cardano.Api (Lovelace)

fundKey :: ClusterEnv -> KeyPair -> [Lovelace] -> IO ()
fundKey cEnv keys funds = do
  let fundAddress = mainnetAddress keys
      toAmt = Coin . fromIntegral
  sendFaucetFundsTo
    nullTracer -- todo: fix tracer to be not `nullTracer`
    (nodeSocket cEnv)
    (supportDir cEnv)
    [(fundAddress, toAmt v) | v <- funds]



module PlutipDistributeFunds (Lovelace(Lovelace), fundKey)where

import Cardano.BM.Data.Tracer (nullTracer)
import Cardano.Wallet.Primitive.Types.Coin (Coin (Coin))

import PlutipCluster (
  sendFaucetFundsTo,
 )

import PlutipTypes (ClusterEnv, nodeSocket, supportDir)

import PlutipKeys (KeyPair, mainnetAddress)
import Cardano.Api (Lovelace(Lovelace))

-- | Fund key's enterprise address with a transaction spending faucet funds.
fundKey :: ClusterEnv -> KeyPair -> [Lovelace] -> IO ()
fundKey cEnv keys funds = do
  let fundAddress = mainnetAddress keys
      toAmt = Coin . fromIntegral
  sendFaucetFundsTo
    nullTracer -- todo: fix tracer to be not `nullTracer`
    (nodeSocket cEnv)
    (supportDir cEnv)
    [(fundAddress, toAmt v) | v <- funds]


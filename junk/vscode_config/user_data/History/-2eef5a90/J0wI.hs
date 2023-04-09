{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import BotPlutusInterface qualified
import BotPlutusInterface.Config qualified as BotPlutusInterface
import BotPlutusInterface.Types (
  HasDefinitions (..),
  SomeBuiltin (..),
  endpointsToSchemas,
 )
import Cardano.PlutusExample.NFT
import Data.Aeson.TH (defaultOptions, deriveJSON)
import Playground.Types (FunctionSchema)
import Schema (FormSchema)
import Prelude

instance HasDefinitions MintNFTContracts where
  getDefinitions :: [MintNFTContracts]
  getDefinitions = []

  getSchema :: MintNFTContracts -> [FunctionSchema FormSchema]
  getSchema _ = endpointsToSchemas @NFTSchema

  getContract :: (MintNFTContracts -> SomeBuiltin)
  getContract = \case
    MintNFT p ->
      SomeBuiltin $
        mintNft p

newtype MintNFTContracts = MintNFT MintParams
  deriving stock (Show)

$(deriveJSON defaultOptions ''MintNFTContracts)

main :: IO ()
main = do
<<<<<<< HEAD
  protocolParams <-
    fromMaybe (error "protocol.json file not found") . JSON.decode
      <$> LazyByteString.readFile "protocol.json"
  let pabConf =
        PABConfig
          { pcCliLocation = Local
          , pcNetwork = Testnet (NetworkMagic 1097911063)
          , pcChainIndexUrl = BaseUrl Http "localhost" 9083 ""
          , pcPort = 9080
          , pcProtocolParams = protocolParams
          , pcTipPollingInterval = 10_000_000
          , pcOwnPubKeyHash = "0f45aaf1b2959db6e5ff94dbb1f823bf257680c3c723ac2d49f97546"
          , pcOwnStakePubKeyHash = Nothing
          , pcScriptFileDir = "./scripts"
          , pcSigningKeyFileDir = "./signing-keys"
          , pcTxFileDir = "./txs"
          , pcDryRun = False
          , pcLogLevel = Debug
          , pcProtocolParamsFile = "./protocol.json"
          , pcEnableTxEndpoint = True
          , pcMetadataDir = "./metadata"
          , pcCollectStats = False
          , pcCollectLogs = False
          }
=======
  pabConf <-
    either error id
      <$> BotPlutusInterface.loadPABConfig "./pabConfig.value"
>>>>>>> master
  BotPlutusInterface.runPAB @MintNFTContracts pabConf

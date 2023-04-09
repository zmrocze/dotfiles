{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Wallet.Cli (initConnection, initConnectionLocalhost, addCoin, addTransaction, addCoinbase, getStatus, sendTransaction) where

-- 
-- Helper functions to be used from withing ghci for interactive session with wallet app running seperately. 
-- 

import Client (makeConnection, send)
import Network.Socket (Socket, ServiceName)
import Server (Address(Address), readMessage)
import GHC.IO.IOMode (IOMode(ReadWriteMode))
import Wallet.Repl (Command (Command), CommandR (..), CommandJSON (..))
import Data.Aeson (encode, FromJSON, decode, ToJSON)
import BlockType (TXID, PublicAddress, Cent (Cent), BlockReference, Coinbase, Transaction)
import BlockCreation (OwnedUTXO)

-- Maybe at some point we will add some authentication data in here. 
data CliState = CliState {
    cliHandle :: Socket
}

-- Open a socket and project it to Handle 
initConnection :: Address -> IO CliState
initConnection addr = CliState <$> makeConnection addr

initConnectionLocalhost :: ServiceName -> IO CliState
initConnectionLocalhost port = initConnection (Address "localhost" port)

commandToJSON :: Command -> CommandJSON
commandToJSON (Command (AddCoin utxo)) = AddCoinJSON utxo
commandToJSON (Command (AddTransaction tx blockref)) = AddTransactionJSON tx blockref
commandToJSON (Command (SendTransaction pubaddr n)) = SendTransactionJSON pubaddr n
commandToJSON (Command (GetStatus txid)) = GetStatusJSON txid

sendCmd :: CliState -> Command -> IO ()
sendCmd state cmd = send (cliHandle state) (encode $ commandToJSON cmd)

recvAnswer :: FromJSON r => CliState -> IO (Either String r)
recvAnswer state = do
    mmr <- fmap decode <$> readMessage (cliHandle state)
    case mmr of
      Nothing -> return $ Left "cli: Message ends short unexpectedly error.\n"
      Just Nothing -> return $ Left "cli: Answer decoding error.\n"
      Just (Just r) -> return $ Right r

-- Send repl command to wallet app. 
sendRecv :: forall r . (ToJSON r, FromJSON r, Show r) => CliState -> CommandR r -> IO ()
sendRecv state cmd = do 
    sendCmd state (Command cmd)
    
    e <- recvAnswer state
    case e of 
      Left s -> putStr s 
      Right (r :: r) -> print r
    
addCoin :: CliState -> OwnedUTXO -> IO () 
addCoin state coin = sendRecv state (AddCoin coin)
addTransaction :: CliState -> Transaction -> Maybe BlockReference -> IO ()
addTransaction state tx blockRef = sendRecv state (AddTransaction (Right tx) blockRef)
addCoinbase :: CliState -> Coinbase -> Maybe BlockReference -> IO ()
addCoinbase state coinbase blockRef = sendRecv state (AddTransaction (Left coinbase) blockRef)
sendTransaction :: CliState -> PublicAddress -> Cent -> IO ()
sendTransaction state pub n = sendRecv state (SendTransaction pub n) 
getStatus :: CliState -> TXID -> IO ()
getStatus state txid = sendRecv state (GetStatus txid)
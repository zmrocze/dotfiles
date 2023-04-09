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

sendRecv = Repl.sendRecv

-- Open a socket and project it to Handle 
initConnection :: Address -> IO CliState
initConnection addr = CliState <$> makeConnection addr

initConnectionLocalhost :: ServiceName -> IO CliState
initConnectionLocalhost port = initConnection (Address "localhost" port)
    
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
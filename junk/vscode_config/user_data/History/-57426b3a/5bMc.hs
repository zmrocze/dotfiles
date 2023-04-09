{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Wallet.Repl where

import BlockCreation (OwnedUTXO)
import BlockType (Transaction, TXID, Cent, PublicAddress, Coinbase, BlockReference)
import GHC.Generics (Generic)
import Data.Aeson
    ( ToJSON,
      FromJSON,
      eitherDecode,
      encode,
      encode,
      FromJSON,
      decode,
      ToJSON )
import Server
    ( readAllMessages,
      Address,
      acceptSingleClient,
      Address(Address),
      readMessage )
import qualified Data.ByteString.Lazy as Lazy
import Data.Bifunctor (second)
import Network.Socket ( Socket, Socket, ServiceName )
import Wallet.Type (Status)
import Client (makeConnection, send)
import GHC.IO.IOMode (IOMode(ReadWriteMode))

-- Non-GADT type to transform generically to and from JSON data. 
data CommandJSON
    = AddCoinJSON  OwnedUTXO
    | AddTransactionJSON  (Either Coinbase Transaction) (Maybe BlockReference)
    | SendTransactionJSON  PublicAddress Cent
    | GetStatusJSON  TXID
    | GetCoinsJSON
    deriving (Generic)

instance ToJSON CommandJSON
instance FromJSON CommandJSON

-- Type for a command to wallet repl. 
data CommandR response where
    AddCoin         :: OwnedUTXO -> CommandR AddCoinResponse
    AddTransaction  :: Either Coinbase Transaction -> Maybe BlockReference -> CommandR AddTransactionResponse
    SendTransaction :: PublicAddress -> Cent -> CommandR SendTransactionResponse
    GetStatus       :: TXID -> CommandR StatusResponse
    GetCoins        :: CommandR GetCoinsResponse

-- Existentially qualified CommandR type.
data Command = forall r . ToJSON r => Command (CommandR r)

parseCommand :: Lazy.ByteString -> Either String Command
parseCommand = second commandFromJSON . eitherDecode
    where
        commandFromJSON :: CommandJSON -> Command
        commandFromJSON (AddCoinJSON utxo) = Command $ AddCoin utxo
        commandFromJSON (AddTransactionJSON tx blockref) = Command $ AddTransaction tx blockref
        commandFromJSON (SendTransactionJSON pubaddr n) = Command $ SendTransaction pubaddr n
        commandFromJSON (GetStatusJSON txid) = Command $ GetStatus txid
        commandFromJSON GetCoinsJSON = Command GetCoins

commandToJSON :: Command -> CommandJSON
commandToJSON (Command (AddCoin utxo)) = AddCoinJSON utxo
commandToJSON (Command (AddTransaction tx blockref)) = AddTransactionJSON tx blockref
commandToJSON (Command (SendTransaction pubaddr n)) = SendTransactionJSON pubaddr n
commandToJSON (Command (GetStatus txid)) = GetStatusJSON txid
commandToJSON (Command GetCoins) = GetCoinsJSON

data AddCoinResponse
    = AddCoinSuccess
    | AddCoinFail
    deriving (Generic, Show)

instance ToJSON AddCoinResponse
instance FromJSON AddCoinResponse

data AddTransactionResponse
    = AddTransactionSuccess
    | AddTransactionFail
    deriving (Generic, Show)

instance ToJSON AddTransactionResponse
instance FromJSON AddTransactionResponse

data SendTransactionResponse
    = SendedTransaction
    | NotEnoughFunds
    | SendTransactionFailure -- TODO: factor errors out of response types
    deriving (Generic, Show)

instance ToJSON SendTransactionResponse
instance FromJSON SendTransactionResponse

data StatusResponse
    = StatusIs TXID Status
    | GetStatusFailure
    deriving (Generic, Show)

instance ToJSON StatusResponse
instance FromJSON StatusResponse

data GetCoinsResponse
    = AvailableCoins [String]
    | GetCoinsFailure
    deriving (Generic, Show)

instance ToJSON GetCoinsResponse
instance FromJSON GetCoinsResponse

readAllCommands :: Socket -> IO [Either String Command]
readAllCommands sock = map parseCommand <$> readAllMessages sock

processMessages :: Socket -> (String -> IO ()) -> (forall r . CommandR r-> IO r) -> IO ()
processMessages sock log f = do
    cmds <- readAllCommands sock
    tillLeft cmds
    where
        -- loops till left error 
        tillLeft [] = log "repl: Finished processing."
        tillLeft (Left str : es) = log ("repl: Command decoding error: \n" <> str)
        tillLeft (Right (Command c) : es) = do
            resp <- f c
            send sock $ encode resp
            tillLeft es

-- | Serve repl at the address, with supplied log function and handler for repl commands
serveRepl :: Address -> (String -> IO ()) -> (forall r . CommandR r-> IO r) -> IO ()
serveRepl addr log handler = acceptSingleClient addr log $ \sock ->
    processMessages sock log handler

sendCmd :: Socket -> Command -> IO ()
sendCmd sock cmd = send sock (encode $ commandToJSON cmd)

recvAnswer :: FromJSON r => Socket -> IO (Either String r)
recvAnswer sock = do
    mmr <- fmap decode <$> readMessage sock
    case mmr of
      Nothing -> return $ Left "cli: Message ends short unexpectedly error.\n"
      Just Nothing -> return $ Left "cli: Answer decoding error.\n"
      Just (Just r) -> return $ Right r

-- Send repl command to wallet app and print the result.
sendRecv :: forall r . (ToJSON r, FromJSON r, Show r) => Socket -> CommandR r -> IO ()
sendRecv sock cmd = do
    sendCmd sock (Command cmd)

    e <- recvAnswer sock
    case e of
      Left s -> putStr s
      Right (r :: r) -> print r
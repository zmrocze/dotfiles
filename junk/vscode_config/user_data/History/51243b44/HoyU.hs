{-# LANGUAGE DeriveGeneric #-}
module Server where

import Control.Exception          (handle, IOException, finally)
import Control.Concurrent         (ThreadId, forkIO, threadDelay, forkFinally)
import Control.Monad              (forever, replicateM)
import Control.Monad.IO.Class     (MonadIO, liftIO)
import Control.Concurrent.MVar    (withMVar, newMVar, MVar)
import Control.Arrow              (second,Arrow (first))
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import Data.Int         (Int32, Int64)
import Data.Bits ( Bits(shiftL, shiftR), toIntegralSized ) 
import Data.Functor               (void) 
import Text.Read                  (readMaybe)
import System.IO                  (IOMode(ReadMode, ReadWriteMode), hSetBuffering, hClose, 
                                   hGetContents, BufferMode (LineBuffering, NoBuffering), 
                                   hPutStr, Handle, hIsEOF, hGetChar, hFlush)
import System.Environment         (getArgs)
import Network.Socket
-- import Network.Transport.Internal (encodeEnum32, decodeNum32) -- TODO avoid this dependency
import qualified Network.Socket.ByteString.Lazy as NSB
import Control.Exception.Base (bracket)
import Data.Word
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)
import Data.Aeson.Types (FromJSONKey, ToJSONKey)
import GHC.IO (unsafeInterleaveIO)

int64ToByteString :: Int64 -> B.ByteString
int64ToByteString n = B.pack [aaaaaaaa, aaaaaaa, aaaaaa, aaaaa, aaaa, aaa, aa, a]
    where
        a = fromIntegral n :: Word8
        aa = fromIntegral (n `shiftR` 8) :: Word8
        aaa = fromIntegral (n `shiftR` 16) :: Word8
        aaaa = fromIntegral (n `shiftR` 24) :: Word8
        aaaaa = fromIntegral (n `shiftR` 32) :: Word8
        aaaaaa = fromIntegral (n `shiftR` 40) :: Word8
        aaaaaaa = fromIntegral (n `shiftR` 48) :: Word8
        aaaaaaaa = fromIntegral (n `shiftR` 56) :: Word8

byteStringToInt64 :: B.ByteString -> Int64
byteStringToInt64 bytes = case B.unpack bytes of
    [a7, a6, a5, a4, a3, a2, a1, a0]  -> 
        (toEnum (fromEnum a7) :: Int64) `shiftL` 56 + (toEnum (fromEnum a6) :: Int64) `shiftL` 48
        + (toEnum (fromEnum a5) :: Int64) `shiftL` 40 + (toEnum (fromEnum a4) :: Int64) `shiftL` 32
        + (toEnum (fromEnum a3) :: Int64) `shiftL` 24 + (toEnum (fromEnum a2) :: Int64) `shiftL` 16
        + (toEnum (fromEnum a1) :: Int64) `shiftL` 8 + (toEnum (fromEnum a0))


-- A message is preceded with 8 bytes that read as an Int64 state the length of the message in bits.

appendLenBits :: B.ByteString -> B.ByteString
appendLenBits bs = B.append (int64ToByteString (B.length bs)) bs

msgToBytes :: String -> B.ByteString
msgToBytes = appendLenBits . UTF8.fromString

-- Keeps reading untill reads n bytes or returns Nothing if stream ends.
-- Doesn't handle errors. 
keepReading :: Socket -> Int64 -> IO (Maybe B.ByteString)
keepReading sock n = loop n B.empty
    where
        loop 0 bs = return $ Just bs
        loop n bs = do 
            bytes <- NSB.recv sock n
            if B.null bytes then 
                return Nothing
            else do
                let n' = n -  B.length bytes
                loop n' (B.append bs bytes)

-- Reads first 8 bytes to get message size, then reads the message.
-- Returns Nothing if message ends short or first 8 bytes invalid.
readMessage :: Socket -> IO (Maybe B.ByteString)
readMessage sock = do 
    mlenBytes <- keepReading sock 8
    case mlenBytes of 
      Nothing -> return Nothing
      Just lenBytes ->
          let len = byteStringToInt64 lenBytes in
            if len <= 0 then
                return Nothing
            else
                keepReading sock len

-- Lazily read all messages from socket.
readAllMessages :: Socket -> IO [B.ByteString]
readAllMessages sock = do
    mmsg <- readMessage sock
    case mmsg of 
        Nothing -> return []
        Just bs -> do 
            --   lazy IO, list produced on demand
            msgs <- unsafeInterleaveIO $ readAllMessages sock
            return $ bs : msgs 

-- type HostName = String
-- Either a host name e.g., "haskell.org" or a numeric host address string consisting of a dotted decimal IPv4 address or an IPv6 address e.g., "192.168.0.1".

-- type ServiceName = String
-- Either a service name e.g., "http" or a numeric port number.

data Address = Address {hostName :: HostName, serviceName :: ServiceName}
    deriving (Show, Generic, Eq, Ord)

instance ToJSON Address
instance FromJSON Address
instance FromJSONKey Address
instance ToJSONKey Address
    
-- get tcp AddrInfo for given url and port
grabAddressInfo :: Address -> IO AddrInfo
grabAddressInfo address = 
    -- getAddrInfo either returns non empty list or raises IOException
    head <$> getAddrInfo (Just $ defaultHints { addrSocketType = Stream }) 
        (Just $ hostName address) 
        (Just $ serviceName address)


type ServerHandler = SockAddr -> B.ByteString -> IO B.ByteString 

type Miliseconds = Int
timeOutToRecvTCP_FIN :: Miliseconds
timeOutToRecvTCP_FIN = 1000

maxConnections = 5

server :: Address
       -> (String -> IO ())
       -> ServerHandler
       -> IO ()
server servAddr logger handler = withSocketsDo $ do
    bracket (open servAddr) close loop

    where

    open servAddr = do 
        addrinfo <- grabAddressInfo servAddr

        sock <- socket (addrFamily addrinfo) Stream defaultProtocol
        setSocketOption sock ReusePort 1
        

        -- bind it to the address we're listening to
        bind sock (addrAddress addrinfo)

        listen sock maxConnections
        
        return sock

    loop sock = forever (procRequest logger sock)

    -- | Proccess incoming connections
    procRequest :: (String -> IO ()) -> Socket -> IO ThreadId
    procRequest log mastersock =
        do  (connsock, clientaddr) <- accept mastersock  -- gets us new socket
            -- log "server: Client connected."
            procConnection log connsock clientaddr 
                `forkFinally`
                const (gracefulClose connsock timeOutToRecvTCP_FIN)
    
    -- | Process incoming messages
    procConnection :: (String -> IO ()) -> Socket -> SockAddr -> IO ()
    procConnection log connsock clientaddr =
        do  mmsg <- readMessage connsock
            case mmsg of
                Nothing -> 
                    log "server: No valid message read. disconnected."
                Just msg -> do 
                        answer <- handler clientaddr msg
                        NSB.sendAll connsock $ appendLenBits answer
                    -- finally 
                    --     (do answer <- handler clientaddr msg
                    --         NSB.sendAll connsock $ appendLenBits answer)
                    --     (log "server: Client disconnected.")       


acceptSingleClient ::
    Address
    -> (String -> IO ())
    -> (Socket -> IO ())
    -> IO ()
acceptSingleClient servAddr log f = withSocketsDo $ do
    bracket (open servAddr) close main

    where

    open servAddr = do 
        addrinfo <- grabAddressInfo servAddr

        sock <- socket (addrFamily addrinfo) Stream defaultProtocol
        
        setSocketOption sock ReusePort 1

        -- bind it to the address we're listening to
        bind sock (addrAddress addrinfo)

        listen sock 1
        
        return sock

    main sock = do
        (connsock, clientaddr) <- accept sock  -- gets us new socket
        log "repl: Client connected."
        f connsock
            `finally`
            (gracefulClose connsock timeOutToRecvTCP_FIN >> log "repl: Closed a connection.")
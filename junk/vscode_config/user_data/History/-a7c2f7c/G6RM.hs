{-# LANGUAGE DeriveGeneric, ScopedTypeVariables, GeneralizedNewtypeDeriving, RecordWildCards #-}

module BlockType where

import GHC.Generics
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LazyB
import Data.Bits (shiftL, shiftR)
import qualified Data.Binary as Binary
import Data.Aeson ( encode, FromJSON(parseJSON), ToJSON(toJSON) )
import Data.Time.Clock (UTCTime)
import Crypto.Util (bs2i, i2bs_unsized)
import qualified Codec.Crypto.RSA as RSA -- Using RSA instead of elliptic curves because the library was better documented
-- Using RSA instead of elliptic curves because the library was better documented
-- Using RSA instead of elliptic curves because the library was better documented
-- Using RSA instead of elliptic curves because the library was better documented
-- Using RSA instead of elliptic curves because the library was better documented
-- Using RSA instead of elliptic curves because the library was better documented
-- Using RSA instead of elliptic curves because the library was better documented
-- Using RSA instead of elliptic curves because the library was better documented
import Hashing (HashOf, RawHash, shash256, ByteStringJSON (ByteStringJSON, getByteStringBack))
import Control.DeepSeq

-- Indivisible unit of our currency.
newtype Cent = Cent Integer deriving (Show, Generic, Num, Ord, Eq)

instance FromJSON Cent
instance ToJSON Cent

-- Transaction ID. 
type TXID = HashOf (Either Coinbase Transaction)
type PublicAddress = HashOf RSA.PublicKey

-- Transactions are signed by input owners. Thats the signature.
newtype Signature = Signature B.ByteString deriving (Show, Generic, Eq)

-- Serialization achieved by converting ByteString to Integer
instance ToJSON Signature where 
    toJSON (Signature b) = toJSON $ ByteStringJSON b
instance FromJSON Signature where 
    parseJSON v = Signature . getByteStringBack <$> parseJSON v


-- Serialization achieved by converting PublicKey to ByteString (using Binary) and then to Integer
instance ToJSON RSA.PublicKey where 
    toJSON = toJSON . ByteStringJSON . LazyB.toStrict . Binary.encode

instance FromJSON RSA.PublicKey where
    parseJSON v = Binary.decode . LazyB.fromStrict . getByteStringBack <$> parseJSON v

-- Transaction consists of:
-- - Inputs - where the money comes from, references to previous outputs
-- - Outputs - where the money goes
data Transaction = Transaction {
        inputs :: [Input],
        outputs :: [Output]
    } deriving (Show, Generic, Eq)

instance FromJSON Transaction
instance ToJSON Transaction

-- For set of incoming transactions in miner.
instance Ord Transaction where 
    compare a b = compare (shash256 a) (shash256 b)

-- Special transaction with outputs only in which miner spends mined coins.
data Coinbase = Coinbase {
    blockHeight :: Integer, -- included so that coinbase transactions to the same address hash differently
    coinbaseOutputs :: [Output]
    } deriving (Show, Generic)

instance FromJSON Coinbase
instance ToJSON Coinbase

data Output = Output {
    outputDenomination :: Cent,
    ownerPublicAddress :: PublicAddress -- specifies who's money it is now
    } deriving (Show, Generic, Eq)

instance FromJSON Output
instance ToJSON Output

-- Specifies what coins (what references to old outputs) are spend, 
-- secured by owner signature, 
-- owner public key is for verifying
-- TXID, vout - reference stating what output does this input spend
data Input = Input {
    signature :: Signature, 
    signerPublicKey :: RSA.PublicKey, 
    utxoReference :: TXID,  -- reference to UTXO (unspend transaction output)
    vout :: Integer         -- output index in a transaction outputs, reference to UTXO
    } deriving (Show, Generic, Eq)

instance FromJSON Input
instance ToJSON Input

-- First Block in the chain, consists of label only.
newtype Genesis = Genesis String deriving (Show, ToJSON, FromJSON, Eq)
-- instance FromJSON Genesis
-- instance ToJSON Input

-- Reference to previous block in a chain.
type BlockReference = HashOf (Either Genesis BlockHeader)

-- Block meta data.
data BlockHeader = BlockHeader {
        nonce :: Integer,    -- number found by the miner that makes the block hash valid.  
        previousHash :: BlockReference, -- reference to previous block in a chain
        timestamp :: UTCTime,           -- timestamp of the block mining, not important
        rootHash :: RawHash             -- hash of the list of transactions (of a merkle tree)
    } deriving (Show, Generic)

instance FromJSON BlockHeader
instance ToJSON BlockHeader

instance Eq BlockHeader where 
    x == y = shash256 x == shash256 y

instance Ord BlockHeader where 
    compare x y = compare (shash256 x) (shash256 y)

-- Needed to force hash crunching in mining.
instance NFData BlockHeader

data Block = Block {
    blockHeader :: BlockHeader,      -- metadata 
    coinbaseTransaction :: Coinbase, -- special transaction spending mined coins 
    transactions :: [Transaction]    -- all the other transactions included
    } deriving (Show, Generic)

instance Eq Block where 
    x == y = blockHeader x == blockHeader y

instance Ord Block where 
    compare x y = compare (blockHeader x) (blockHeader y)

instance FromJSON Block
instance ToJSON Block

blockNonce :: Block -> Integer
blockNonce = nonce . blockHeader

blockPreviousHash :: Block -> HashOf (Either Genesis BlockHeader)
blockPreviousHash = previousHash . blockHeader

blockTimestamp :: Block -> UTCTime
blockTimestamp = timestamp . blockHeader

blockRootHash :: Block -> RawHash
blockRootHash = rootHash . blockHeader

blockBlockHeight :: Block -> Integer
blockBlockHeight Block{coinbaseTransaction=c, ..} = blockHeight c
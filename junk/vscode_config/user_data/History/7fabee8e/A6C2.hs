{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE FlexibleContexts #-}
module Wallet.Statement where

import BlockType (TXID, BlockReference, Transaction, BlockHeader (BlockHeader), PublicAddress)
import Hashing (HashOf(Hash), RawHash (RawHash))
import Hasql.Statement (Statement (Statement))
import Data.Vector (Vector)
import qualified Hasql.Encoders as E
import qualified Hasql.Decoders as D
import Data.Aeson (FromJSON, decodeStrict', eitherDecodeStrict', ToJSON, encode, toJSON)
import Data.Bifunctor (first)
import Data.Text (pack)
import Data.Functor.Contravariant (contramap)
import Data.Coerce (coerce, Coercible)
import Unsafe.Coerce (unsafeCoerce)
import Data.ByteString (ByteString)
import Data.Int (Int64, Int32)
import Contravariant.Extras (contrazip2, contrazip4, contrazip5)
import Data.Foldable (foldl')
import Wallet.Type (StoredTransaction (StoredTransaction), Status (Validated, Waiting, Discarded))
import qualified Codec.Crypto.RSA as RSA
import Data.Binary (Binary)
import qualified Data.Binary as Binary
import Data.ByteString.Lazy (toStrict, fromStrict)
import qualified Data.Aeson as Aeson


jsonb2aeson :: FromJSON a => D.Value a
jsonb2aeson = D.jsonbBytes (first pack . eitherDecodeStrict')

aeson2jsonb :: ToJSON a => E.Value a
aeson2jsonb = contramap toJSON E.jsonb

rowHash :: D.Row (HashOf a)
rowHash = coerceHash <$> D.column (D.nonNullable D.bytea)
    where
        coerceHash :: ByteString -> HashOf a
        coerceHash = coerce

rowHashNull :: D.Row (Maybe (HashOf a))
rowHashNull = fmap coerceHash <$> D.column (D.nullable D.bytea)
    where
        coerceHash ::  ByteString -> HashOf a
        coerceHash = coerce


-- encodeHash :: E.Params BlockReference
encodeHash :: Coercible b ByteString =>  E.Params b
encodeHash = contramap coerce . E.param . E.nonNullable $ E.bytea

encodeHashNull :: Coercible b ByteString =>  E.Params (Maybe b)
encodeHashNull = contramap coerce . E.param . E.nullable $ E.bytea

txDecoder :: D.Row (TXID, Maybe BlockReference, Aeson.Value, Status, Bool)
txDecoder = (,,,,)
    <$> rowHash
    <*> rowHashNull
    <*> D.column (D.nonNullable jsonb2aeson)
    <*> rowStatus
    <*> D.column (D.nonNullable D.bool)

rowStatus :: D.Row Status
rowStatus = D.column (D.nonNullable (D.enum str2status))

    where
        str2status "validated" = Just Validated
        str2status "waiting" = Just Waiting
        str2status "discarded" = Just Discarded
        str2status _ = Nothing

encodeStatus :: E.Params Status
encodeStatus = E.param . E.nonNullable $ E.enum status2str
    where
        status2str Validated = "validated"
        status2str Waiting   = "waiting"
        status2str Discarded = "discarded"


selectTxByStatus :: Statement Status (Vector (TXID, Maybe BlockReference, Aeson.Value, Status, Bool))
selectTxByStatus = Statement sql encodeStatus (D.rowVector txDecoder) True
    where
        sql = "select (tx_id, tx_block_id, tx_data, tx_status, tx_is_coinbase) from transaction where tx_status = ($1 :: transaction_status)"


-- selectTxByBlock :: Statement BlockReference (Vector (TXID, Maybe BlockReference, Aeson.Value, Status, Bool))
-- selectTxByBlock = Statement sql encodeHash (D.rowVector txDecoder) True
--     where
--         sql = "select (tx_id, tx_block_id, tx_data, tx_status, tx_is_coinbase) from transaction where tx_block_id is not null and tx_block_id = $1"

selectTxIdByBlock :: Statement BlockReference (Vector TXID)
selectTxIdByBlock = Statement sql encodeHash (D.rowVector rowHash) True
    where
        sql = "select tx_id from transaction where (tx_block_id is not null) and (tx_block_id = $1)"

-- updateTxStatus :: Statement (Status, TXID) ()
-- updateTxStatus = Statement sql (contrazip2 encodeStatus encodeHash) D.noResult True
--     where
--         sql = "update transaction set tx_status = $1 where tx_id = $2"


-- Update txs statuses for transactions from block
updateTxStatusByBlock :: Statement (Status, BlockReference) Int64
updateTxStatusByBlock = Statement sql (contrazip2 encodeStatus encodeHash) D.rowsAffected True
    where 
        sql = "update transaction set tx_status = ($1 :: transaction_status) where tx_block_id = $2"


updateTxStatusMany :: Statement (Status, Vector TXID) ()
updateTxStatusMany = Statement sql (contrazip2 encodeStatus (vector $ contramap coerce E.bytea)) D.noResult True
    where
        sql = "update transaction set tx_status = ($1 :: transaction_status) from unnest($2) as t(num) where t.num = tx_id"

vector =
    E.param .
    E.nonNullable .
    E.array .
    E.dimension foldl' .
    E.element .
    E.nonNullable

-- Insert fixed block header if not already inserted.
addFixedBlockHeader :: Statement (BlockReference , BlockHeader) ()
addFixedBlockHeader = Statement sql e D.noResult True
    where
        sql = "insert into fixed_header values ($1, $2) \
        \ on conflict (block_id) do nothing"
        e = contrazip2 encodeHash (E.param . E.nonNullable $ aeson2jsonb)

selectFixedCount :: Statement () Int64
selectFixedCount = Statement sql E.noParams (D.singleRow . D.column . D.nonNullable $ D.int8) True
    where
        sql = "select count(*) from fixed_header"

-- Inserts a transaction, do nothing on conflicting txid (that is when transaction already in db)
insertTransaction :: Statement (TXID, Maybe BlockReference, Aeson.Value, Status, Bool) ()
insertTransaction = Statement sql e D.noResult True
    where
        sql = "insert into transaction (tx_id, tx_block_id, tx_data, tx_status, tx_is_coinbase) \
        \ values ($1, $2, $3, $4 :: transaction_status, $5) \
        \ on conflict (tx_id) do nothing"
        e = contrazip5
            encodeHash
            encodeHashNull
            (E.param . E.nonNullable $ aeson2jsonb)
            encodeStatus
            (E.param . E.nonNullable $ E.bool)

-- -- Inserts a transaction error on conflict:
--         sql = "insert into transaction (tx_id, tx_block_id, tx_data, tx_status, tx_is_coinbase) \
--         \ values ($1, $2, $3, $4 :: transaction_status, $5)"


encodeBinary :: Binary a => E.Params a
encodeBinary = contramap (toStrict . Binary.encode) . E.param . E.nonNullable $ E.bytea

rowBinary :: Binary a => D.Row a
rowBinary = Binary.decode . fromStrict <$> (D.column . D.nonNullable $ D.bytea)

insertOwnedKeys :: Statement (TXID , Int32, RSA.PublicKey, RSA.PrivateKey, Cent, PublicAddress) ()
insertOwnedKeys = Statement sql e D.noResult True
    where
        sql = "insert into owned_keys (keys_tx_id, vout, pub_key, priv_key) values ($1, $2, $3, $4)"
        e = contrazip4 encodeHash (E.param $ E.nonNullable E.int4) encodeBinary encodeBinary

selectStatus :: Statement TXID Status
selectStatus = Statement sql encodeHash (D.singleRow rowStatus) True
    where
        sql = "select tx_status from transaction where tx_id = $1"

-- selectTxsForAmount :: Statement Int64 (Vector (TXID, Int32, RSA.PublicKey, RSA.PrivateKey, Cent, PublicAddress))
-- Effective implementation for such query would need changing the data model by adding outputs, inputs tables and destructing transaction into components
-- Let's go with bruteforce solution in a hope that wallet only stores medium amounts of transactions and not that often sends transactions. 

selectOwnedByStatus :: Statement Status (Vector (TXID, Int32, RSA.PublicKey, RSA.PrivateKey, Aeson.Value, Bool))
selectOwnedByStatus = Statement sql encodeStatus (D.rowVector d) True
    where
        sql = "select tx_id, vout, pub_key, priv_key, tx_data, tx_is_coinbase \
        \ from transaction, owned_keys \
        \ where ((keys_tx_id = tx_id) and (tx_status = ($1 :: transaction_status)))"
        nonNullableColumn = D.column . D.nonNullable
        d = (,,,,,) 
            <$> rowHash
            <*> nonNullableColumn D.int4
            <*> rowBinary
            <*> rowBinary
            <*> nonNullableColumn jsonb2aeson
            <*> nonNullableColumn D.bool

updateBlockRef :: Statement (BlockReference , Vector TXID) Int64
updateBlockRef = Statement sql e D.rowsAffected True
    where
        sql = "update transaction set tx_block_id = $1 from unnest($2) as t(num) where t.num = tx_id"
        e = contrazip2 encodeHash (vector $ contramap coerce E.bytea)

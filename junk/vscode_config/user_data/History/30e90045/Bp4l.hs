{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
module Ledger.Constrs.TxConstrs_new where
import Ledger
import Ledger.Constraints.OffChain (ScriptLookups (slOtherData))
import qualified Data.Map as Map

-- | Constrs on transactions that want to spend script outputs
data TxConstr =
      MustHashDatum DatumHash Datum
    -- ^ Like 'MustHashDatum', but the hash of the 'Datum' is computed automatically.
    | MustValidateIn POSIXTimeRange
    -- ^ The transaction's validity range must be set with the given 'POSIXTimeRange'.
    -- | MustBeSignedBy PaymentPubKeyHash
    | MustPayToOtherScript ValidatorHash (Maybe StakeValidatorHash) Datum Value
    

-- data BuildWith a = BuildWith

data TxConstr' a b where
    MustHashDatum' :: DatumHash -> Datum -> TxConstr' a a
    -- | Like 'MustHashDatum', but the hash of the 'Datum' is computed automatically.
    MustValidateIn' :: POSIXTimeRange -> TxConstr' a a
    -- -- | The transaction's validity range must be set with the given 'POSIXTimeRange'.
    -- MustBeSignedBy' :: PaymentPubKeyHash -> TxConstr' a (Maybe (PaymentPubKey, Signature) -> a)
    
    -- | The transaction must create a transaction output with a script address.
    MustPayToOtherScript' :: ValidatorHash -> (Maybe StakeValidatorHash) -> Datum -> Value -> TxConstr' a (Validator -> a)

data TxConstr'List b where 
    Nil :: TxConstr'List a a
    Cons :: forall a . TxConstr' b c -> TxConstr'List a b -> TxConstr'List a c

data Ex2 f = forall a b . Ex2 (f a b)

toConstrList :: [TxConstrEx] -> Ex2 TxConstr'List
toConstrList [TxConstrEx x] = Ex2 $ Cons x Nil
toConstrList (TxConstrEx x@(MustHashDatum' dth dt) : xs) = 
    let Ex2 xs' = toConstrList xs
        in Ex2 $ Cons x xs'
toConstrList _ = undefined 

tx'2tx :: TxConstr' a b -> TxConstr
tx'2tx (MustHashDatum' datumhash datum) = MustHashDatum datumhash datum
tx'2tx (MustValidateIn' timerange) = MustValidateIn timerange
-- tx'2tx (MustBeSignedBy' pubkeyhash) = MustBeSignedBy pubkeyhash
tx'2tx (MustPayToOtherScript' vlhash mstake dt val) = MustPayToOtherScript vlhash mstake dt val

data TxConstrEx = forall a b . TxConstrEx (TxConstr' a b)

tx2tx' :: TxConstr -> TxConstrEx
tx2tx' (MustHashDatum datumhash datum) = TxConstrEx $ MustHashDatum' datumhash datum
tx2tx' (MustValidateIn timerange) = TxConstrEx $ MustValidateIn' timerange
-- tx2tx' (MustBeSignedBy pubkeyhash) = TxConstrEx $ MustBeSignedBy' pubkeyhash
tx2tx' (MustPayToOtherScript vlhash mstake dt val) = TxConstrEx $ MustPayToOtherScript' vlhash mstake dt val

-- a :: Signature -> Tx

buildTx :: TxConstr' a b  -> (ScriptLookups c -> a) -> (ScriptLookups c -> b)
buildTx (MustHashDatum' datumhash datum) a sl = a $ sl {slOtherData = Map.insert datumhash datum (slOtherData sl) }
buildTx (MustValidateIn' timerange) a sl = a sl -- what to do here?
-- buildTx (MustBeSignedBy' pubKeyH) a sl = \case
--     Nothing -> a sl
--     Just (pubkey, signature) ->
--         if paymentPubKeyHash pubkey == pubKeyH then 
--             a $ _ 
--         else 
--             _
buildTx (MustPayToOtherScript' validatorHash mStakeHash datum value) a sl = \validator -> 
    _
    

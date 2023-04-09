{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Data.Aeson
import Data.Aeson.KeyMap ( toList, fromList )
import Data.Text (Text, intercalate)
import Data.Scientific (Scientific)
import Control.Monad.State (StateT (runStateT), modify', MonadTrans (lift))
import Data.Aeson.Key (toText, fromText)
import System.Environment (getArgs)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Data.Bifunctor (Bifunctor(second))
import Data.Bifunctor.Swap (swap)

data Leaf
    = S Text
    | N Scientific
    | B Bool
    | Nul

-- jsonPaths :: Value -> Maybe [([Text], Leaf)]
-- jsonPaths = _
--     where recurse path = \case
--             (Array a)  -> Nothing
--             (Object o) -> fmap concat $ mapM (\(k, v) -> recurse (k : path) v) $ toList o
--             (String s) -> Just (path, S s)
--             (Number n) -> _
--             (Bool b)   -> _
--             Null       -> _

jsonPaths :: Value -> Maybe [(Leaf, [Text])]
jsonPaths v = mapM (\(m, l) -> (,l) <$> m) (runStateT (runMaybeT $ recurse v) [])
    where
        recurse :: Value -> MaybeT (StateT [Text] []) Leaf
        recurse = \case
            (Array a)  -> MaybeT $ return Nothing
            (Object o) -> (lift . lift) (toList o) >>= \(k, v) -> modify' (toText k :) >> recurse v
            (String s) -> return (S s)
            (Number n) -> return (N n)
            (Bool b)   -> return (B b)
            Null       -> MaybeT $ return Nothing

-- prettyEntry :: Leaf -> [Text] -> Text
-- prettyEntry _ ts = intercalate "-" ts

main = do
    [fp] <- getArgs
    val <- decodeFileStrict' fp
    case val >>= jsonPaths of
        Nothing -> error "unexpected json or not json at all" 
        Just ls -> let 
            kvs = map ( swap . second ( fromText . intercalate "-" . reverse)) ls
            newjson = fromList kvs
            in return ()
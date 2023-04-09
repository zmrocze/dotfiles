{-# LANGUAGE LambdaCase #-}

module Main where

import Data.Aeson
import Data.Aeson.KeyMap ( toList )
import Data.Text (Text)
import Data.Scientific (Scientific)
import Control.Monad.State (StateT (runStateT), modify', MonadTrans (lift))
import Data.Aeson.Key (toText)
import System.Environment (getArgs)
import Control.Monad.Trans.Maybe (MaybeT(..))

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
jsonPaths v =runStateT $ (sequenceA $ (runMaybeT $ recurse v)) []
    where
        recurse :: Value -> MaybeT (StateT [Text] []) Leaf 
        recurse = \case
            (Array a)  -> MaybeT $ return Nothing
            (Object o) -> (lift . lift) (toList o) >>= \(k, v) -> modify' (toText k :) >> recurse v
            (String s) -> return (S s)
            (Number n) -> return (N n)
            (Bool b)   -> return (B b)
            Null       -> MaybeT $ return Nothing

prettyEntry :: Leaf -> [Text] -> Text
prettyEntry _ = _

main = do
    [fp] <- getArgs
    val <- decodeFileStrict' fp
    _ $ json_paths val

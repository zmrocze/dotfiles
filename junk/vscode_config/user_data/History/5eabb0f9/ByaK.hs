{-# LANGUAGE LambdaCase #-}
module Main where

data Tree a 
  -- = Root (Tree a) (Tree a)
  = Edge a (Tree a) (Tree a)
  | Seal 
  deriving (Show, Eq)

foldTree :: (a -> b -> a) -> a -> Tree b -> a
foldTree f zero Seal = zero
foldTree f zero (Edge b l r)  = let 
  a = foldTree f zero l
  a1 = foldTree f a r
  in f a1 b

-- foldl :: (a -> b -> a) -> a -> [b] -> a

loop :: (a -> Either a b) -> a -> b
loop f a = case f a of 
  Left a1 -> loop f a1
  Right b -> b

foldTreeLoop :: (a -> b -> a) -> a -> Tree b -> a

-- foldTreeLoop f a t = let 
--   x  = loop (\case
--     Seal -> Right a
--     Edge e l r -> Left  )

-- >=> :: (c -> m a) -> (a -> m b) -> (c -> m b)
-- >>=

-- return b >>= \a -> ma a == ma b

-- ma >>= return = ma

-- return >=> ma == ma
-- ma >=> return == ma

-- (ma >=> mb) >=> mc == ma >=> (mb >=> mc)
-- (ma >>= \a -> mb) >>= \b -> mc == ma >>= (\a -> mb >>= \b -> mc)

foldTreeLoop f a t = loop (\case
  (acc, Seal, Seal : ts) -> Left (acc, Seal, ts)
  (acc, Seal, (Edge e l r) : ts) -> Left (f acc e, l, r : ts)
  (acc, Edge e l r, ts) -> Left (f acc e, l, r : ts)
  (acc, Seal, []) -> Right acc ) 
  (a, t, [])


main :: IO ()
main = putStrLn "Hello, Haskell!"

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
foldTreeLoop f a Seal = a let 
  x  = loop (\case
    Seal -> Right a
    Edge e l r -> Left  )

main :: IO ()
main = putStrLn "Hello, Haskell!"

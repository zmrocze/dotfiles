{-# LANGUAGE LambdaCase #-}
module Main where

main :: IO ()
main = putStrLn "Hello, Haskell!"


data Atom r x
  = Atom r [x]
  deriving (Show, Ord, Eq)

data GFFormula r x
  = Forall (Atom r x) (GFFormula r x)
  | Not (GFFormula r x)
  | And (GFFormula r x) (GFFormula r x)
  | Or (GFFormula r x) (GFFormula r x)
  | Literal Bool (Atom r x)
  deriving (Show, Ord, Eq)

newtype NNFed r x = NNFed (GFFormula r x)
  deriving (Show, Ord, Eq)

nnf :: GFFormula r x -> NNFed GFFormula r x
nnf = \case
  Not (Literal b p) -> Literal (not b) p
  Not (Not g) -> nnf g
  Not (g `And` h) -> nnf (Not g) `Or` nnf (Not h) 
  Not (g `Or` h) -> nnf (Not g) `And` nnf (Not h)


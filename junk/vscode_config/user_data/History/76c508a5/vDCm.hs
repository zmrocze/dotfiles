{-# LANGUAGE LambdaCase #-}

module Main where

import Data.Functor.Foldable

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

newtype NNFed f = NNFed f
  deriving (Show, Ord, Eq)

phi :: GFFormula r x -> GFFormula r x
phi = undefined

  where 
    
    nnf :: GFFormula r x -> GFFormula r x
    nnf = \case
      Not (Literal b p) -> Literal (not b) p
      Not (Not g) -> nnf g
      Not (g `And` h) -> nnf (Not g) `Or` nnf (Not h) 
      Not (g `Or` h) -> nnf (Not g) `And` nnf (Not h)


    struct :: GFFormula r x -> GFFormula r x
    struct = \case  
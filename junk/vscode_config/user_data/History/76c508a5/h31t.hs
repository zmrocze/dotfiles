{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DerivingStrategies #-}

module Main where

main :: IO ()
main = putStrLn "Hello, Haskell!"


data Atom r x
  = Atom r [x]
  deriving (Show, Ord, Eq)

data GFFormula a
  = Forall a (GFFormula a)
  | Not (GFFormula a)
  | And (GFFormula a) (GFFormula a)
  | Or (GFFormula a) (GFFormula a)
  | Literal Bool a
  deriving (Show, Ord, Eq)
  deriving (Functor)

newtype NNFed f = NNFed f
  deriving (Show, Ord, Eq)

phi :: GFFormula r x -> GFFormula r x
phi = undefined

  where 
    
    nnf :: GFFormula (Atom r x) -> GFFormula (Atom r x)
    nnf = \case
      Not (Literal b p) -> Literal (not b) p
      Not (Not g) -> g
      Not (g `And` h) -> nnf (Not g) `Or` nnf (Not h) 
      Not (g `Or` h) -> nnf (Not g) `And` nnf (Not h)
      And g h -> And (nnf g) (nnf h)
      Or g h -> Or (nnf g) (nnf h)
      l@(Literal _ _) -> l
      

    struct :: GFFormula r x -> GFFormula r x
    struct = _
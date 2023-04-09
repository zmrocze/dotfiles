{-# LANGUAGE LambdaCase #-}

module Main where

main :: IO ()
main = putStrLn "Hello, Haskell!"


data Atom r x
  = Atom r [x]
  deriving (Show, Ord, Eq)

data GFFormula a
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
      Not (Not g) -> g
      Not (g `And` h) -> nnf (Not g) `Or` nnf (Not h) 
      Not (g `Or` h) -> nnf (Not g) `And` nnf (Not h)
      And g h -> And (nnf g) (nnf h)
      Or g h -> Or (nnf g) (nnf h)
      nnf l@(Literal _ _) -> l 

    struct :: GFFormula r x -> GFFormula r x
    struct = _
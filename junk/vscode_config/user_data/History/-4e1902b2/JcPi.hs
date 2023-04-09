
module NNF where

data NNFed a
  = Forall a (GFFormula a)
  | Exist a (GFFormula a)
  | Not (GFFormula a)
  | And (GFFormula a) (GFFormula a)
  | Or (GFFormula a) (GFFormula a)
  | AtomA a
  deriving (Show, Ord, Eq)
  deriving (Functor)

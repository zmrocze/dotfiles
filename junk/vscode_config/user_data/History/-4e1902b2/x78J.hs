{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveFunctor #-}

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

nnf :: GFFormula (Atom r x) -> GFFormula (Atom r x)
nnf = \case
    Not (Atom b p) -> Atom (not b) p
    Not (Not g) -> g
    Not (g `And` h) -> nnf (Not g) `Or` nnf (Not h) 
    Not (g `Or` h) -> nnf (Not g) `And` nnf (Not h)
    And g h -> And (nnf g) (nnf h)
    Or g h -> Or (nnf g) (nnf h)
    l@(Atom _ _) -> l
    Not (Forall p g) -> Exist p (nnf (Not g))
    Not (Exist p g) -> Forall p (nnf (Not g))
    Exist p g -> Exist p $ nnf g
    Forall p g -> Forall p $ nnf g

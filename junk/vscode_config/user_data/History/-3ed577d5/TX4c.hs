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
    GF.Not (GF.Atom b p) -> Atom (not b) p
    GF.Not (GF.Not g) -> g
    GF.Not (g `GF.And` h) -> nnf (Not g) `Or` nnf (Not h) 
    GF.Not (g `GF.Or` h) -> nnf (Not g) `And` nnf (Not h)
    GF.And g h -> And (nnf g) (nnf h)
    GF.Or g h -> Or (nnf g) (nnf h)
    l@(GF.Atom _ _) -> l
    GF.Not (GF.Forall p g) -> Exist p (nnf (Not g))
    GF.Not (GF.Exist p g) -> Forall p (nnf (Not g))
    GF.Exist p g -> Exist p $ nnf g
    GF.Forall p g -> Forall p $ nnf g

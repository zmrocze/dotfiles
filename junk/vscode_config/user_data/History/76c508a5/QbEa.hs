{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveFunctor #-}

module Main where

main :: IO ()
main = putStrLn "Hello, Haskell!"



-- phi :: GFFormula a  -> GFFormula a 
-- phi = undefined

--   where 
    
--     nnf :: GFFormula (Atom r x) -> GFFormula (Atom r x)
--     nnf = \case
--       Not (Atom b p) -> Atom (not b) p
--       Not (Not g) -> g
--       Not (g `And` h) -> nnf (Not g) `Or` nnf (Not h) 
--       Not (g `Or` h) -> nnf (Not g) `And` nnf (Not h)
--       And g h -> And (nnf g) (nnf h)
--       Or g h -> Or (nnf g) (nnf h)
--       l@(Atom _ _) -> l
--       Not (Forall p g) -> Exist p (nnf (Not g))
--       Not (Exist p g) -> Forall p (nnf (Not g))
--       Exist p g -> Exist p $ nnf g
--       Forall p g -> Forall p $ nnf g
    
--     struct :: GFFormula (Atom r x) -> GFFormula (Atom r x)
--     struct = \case
--       l@(Atom _ _) -> [l]
--       Not g -> (Not )
--       Exist p g -> Forall p (new g) : (struct g)
--       Forall p g -> Forall p $ nnf g
  
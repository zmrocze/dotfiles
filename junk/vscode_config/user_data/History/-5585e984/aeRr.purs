module Api
  (
    square
  )
  where

import Prelude
import Data.Function.Uncurried (Fn1, mkFn1)


square :: Fn1 Int Int
square = mkFn1 $ \n -> n*n

alwaysSucceedsString = 
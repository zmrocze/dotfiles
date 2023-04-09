module Prelude.HshPrelude (
  module Prelude,
  module Prelude.HshPrelude,
  module Data.Foldable,
  module Data.Traversable,
  module Data.Either,
  module Data.Maybe,
  module Control.Arrow,
  module Control.Monad,
  module Data.Bifunctor,
  module Data.List,
  module Numeric,
  module Data.Char,
  module Turtle.Prelude
) where

import Prelude
import Data.Foldable
import Data.Traversable
import Data.Either
import Data.Maybe
import Control.Arrow hiding (first, second) -- we will get them (but different) from bifunctor
import Control.Monad
import Data.Bifunctor
import Data.List
import Data.Char
import Numeric
import Turtle.Prelude hiding (sort, sortOn, sortBy, nub, find)
import qualified Language.Haskell.Interpreter as I
-- import GHC.Utils.Misc (nTimes)

langExtensions = [I.LambdaCase, I.OverloadedStrings]
preludePath = "/home/zmrocze/code/haskell/hsh/src/Hsh/HshPrelude.hs"
extraQualifiedImports = [
  ("Data.Map", Just "M"),
  ("Turtle.Prelude", Just "T")
  ]

nTimes 0 _ = id
nTimes 1 f = f
nTimes n f = f . nTimes (n-1) f 

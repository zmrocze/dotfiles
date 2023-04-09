module Api
  (
    square
  , always_succeeds
  )
  where

import Prelude
import Data.Function.Uncurried (Fn1, mkFn1)
import MLabsPlutusTemplate.Scripts (always_succeeds)
-- import Contract.JsSdk
--   ( runContractJS
--   )
import Contract.

square :: Fn1 Int Int
square = mkFn1 $ \n -> n*n

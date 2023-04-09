module MlabsPlutusTemplate.Api
  (
    square
  -- , module Export
  )
  where

import Prelude
import Data.Function.Uncurried (Fn1, mkFn1)

-- import MLabsPlutusTemplate.Scripts (always_succeeds) as Export

-- For this we need newer ctl revision
-- import Contract.JsSdk
--   ( runContractJS
--   )
-- import Data.Int (pow)

square :: Fn1 Int Int
square = mkFn1 $ \n -> n * n

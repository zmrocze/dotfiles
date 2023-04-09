module MlabsPlutusTemplate.Api
  (
    square
  -- , module Export
  )
  where

import Prelude
import Data.Function.Uncurried (Fn1, mkFn1)

-- import MLabsPlutusTemplate.Scripts (always_succeeds) as Export

-- For this we need newer ctl revision, that module seems useful
-- import Contract.JsSdk
--   ( runContractJS
--   )


square :: Fn1 Int Int
square = mkFn1 $ \n -> n * n

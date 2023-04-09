module MlabsPlutusTemplate.Api
  (
    square
  -- , module Export
  )
  where

import Prelude
import Data.Function.Uncurried (Fn1, mkFn1)

-- For this import we need BROWSER_RUNTIME webpack setup like in ctl (its no problem)
-- import MLabsPlutusTemplate.Scripts (always_succeeds) as Export

-- For this import we need newer ctl revision, that module seems useful
-- import Contract.JsSdk
--   ( runContractJS
--   )

-- TODO: Webpack errors if we export stuff from CTL including WASM (like below).
-- The problem is WASM needs to be loaded async
import Contract.Scripts (applyArgs) as Export

-- applyArgs :: PlutusScript -> Array PlutusData -> Either ApplyArgsError PlutusScript
-- applyArgs = applyArgs

square :: Fn1 Int Int
square = mkFn1 $ \n -> n * n

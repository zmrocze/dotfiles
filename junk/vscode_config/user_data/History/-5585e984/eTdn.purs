module Api where

import Data.Function.Uncurried (Fn1, mkFn1)

initialize :: Fn1 Int Int
initialize = mkContractEnvJS
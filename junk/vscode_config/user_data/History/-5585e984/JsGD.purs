module MlabsPlutusTemplate.Api
  (
    square
  , module Export
  )
  where

import Prelude
import Data.Function.Uncurried (Fn1, mkFn1)
import MLabsPlutusTemplate.Scripts (always_succeeds) as Export

-- import Contract.JsSdk
--   ( runContractJS
--   )

contract :: Contract () Unit
contract = do
  logInfo' "Welcome to CTL! Your wallet's payment PubKey hash is:"
  logInfo' <<< show =<< ownPaymentPubKeyHash

square :: Fn1 Int Int
square = mkFn1 $ \n -> n*n

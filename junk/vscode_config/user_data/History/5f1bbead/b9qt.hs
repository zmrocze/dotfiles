module Main (main) where

import Spec.Integration qualified as Integration

-- import Spec.Test.Plutip.BotPlutusInterface qualified as BotInterface
-- import Spec.Test.Plutip.BotPlutusInterface qualified as BotInterface
import Test.Tasty (defaultMain, testGroup, localOption)
import Test.Tasty.Ingredients.ConsoleReporter (UseColor(Always))

main :: IO ()
main =
  defaultMain $ localOption Always $
    testGroup
      "tests"
      -- FIXME: both `Integration.test` and `BotInterface.test`
      -- start own cluster to run tests, probably, need better solution in future
      [ Integration.test
      -- , BotInterface.test
      ]

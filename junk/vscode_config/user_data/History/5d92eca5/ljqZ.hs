module Main (main) where

import Spec.Integration qualified as Integration

import Spec.Test.Plutip.BotPlutusInterface qualified as BotInterface
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main =
  defaultMain $
    testGroup
      "tests"
      [ Integration.test
      ]

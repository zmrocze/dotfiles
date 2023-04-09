module Main (main) where

import Spec.ClusterStart qualified as ClusterStart

import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main =
  defaultMain $
    testGroup
      "tests"
      [ Integration.test
      ]

module Main (main) where

import GHC.List (List)
import System.IO
import Test.Tasty
import Test.Tasty.HUnit

import Confer.CLI.Cmd.CheckTest qualified as CheckTest
import Utils

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  spec <- traverse (\comp -> runTestEff comp) specs
  defaultMain $
    testGroup
      "Confer Tests"
      spec

specs :: List (TestEff TestTree)
specs =
  [ CheckTest.spec
  ]

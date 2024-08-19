module Main (main) where

import GHC.List (List)
import System.IO
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Runners.Reporter qualified as Reporter

import Confer.CLI.Cmd.CheckTest qualified as CheckTest
import Utils (TestEff)
import Utils qualified

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  spec <- traverse (\comp -> Utils.runTestEff comp) specs
  defaultMainWithIngredients
    [Reporter.ingredient]
    $ testGroup
      "Confer Tests"
      spec

specs :: List (TestEff TestTree)
specs =
  [ CheckTest.spec
  ]

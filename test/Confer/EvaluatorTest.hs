module Confer.EvaluatorTest where

import Control.Monad
import Data.Function
import Data.List.NonEmpty qualified as NE
import Data.Vector qualified as Vector
import Effectful
import Effectful.Error.Static (Error)
import Effectful.Error.Static qualified as Error
import Effectful.FileSystem
import Effectful.FileSystem qualified as FileSystem
import System.FilePath qualified as FilePath
import System.IO.Temp qualified as Temporary
import System.OsPath
import System.OsPath qualified as OsPath
import Test.Tasty (TestTree)
import Test.Tasty.HUnit ()

import Confer.Config.Evaluator
import Utils

spec :: TestEff TestTree
spec =
  testThese
    "Evaluator Checks"
    [ testThis "Empty configuration" testEmptyConfigurationoError
    ]

testEmptyConfigurationoError :: TestEff ()
testEmptyConfigurationoError = do
  Temporary.withSystemTempDirectory "empty-config.ext" $ \directory -> do
    copyFile "./test/fixtures/empty-config.lua" (directory <> "/empty-config.lua")
    filepath <- liftIO $ encodeUtf $ directory <> "/empty-config.lua"
    result <- assertRight =<< loadConfiguration False filepath
    assertEqual result Vector.empty

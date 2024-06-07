module Utils where

import Data.Function ((&))
import Data.List.NonEmpty (NonEmpty (..))
import Effectful
import Effectful.Error.Static (Error)
import Effectful.Error.Static qualified as Error
import Effectful.FileSystem (FileSystem)
import Effectful.FileSystem qualified as FileSystem
import GHC.Stack
import Test.Tasty (TestTree)
import Test.Tasty qualified as Test
import Test.Tasty.HUnit qualified as Test

import Confer.CLI.Errors
import Confer.Effect.Symlink

type TestEff =
  Eff [FileSystem, Error CLIError, IOE]

runTestEff
  :: Eff [FileSystem, Error CLIError, IOE] a
  -> IO a
runTestEff action = do
  result <-
    action
      & FileSystem.runFileSystem
      & Error.runErrorNoCallStack
      & runEff
  case result of
    Left e -> Test.assertFailure $ show e
    Right a -> pure a

assertFailure :: HasCallStack => MonadIO m => String -> m ()
assertFailure = liftIO . Test.assertFailure

assertWrongTarget :: HasCallStack => Either SymlinkError () -> TestEff ()
assertWrongTarget (Right _) = assertFailure "Did not return Left"
assertWrongTarget (Left (WrongTarget{})) = pure ()
assertWrongTarget (Left e) = assertFailure $ "Returned: " <> show e

assertDoesNotExist :: HasCallStack => Either SymlinkError () -> TestEff ()
assertDoesNotExist (Right _) = assertFailure "Did not return Left"
assertDoesNotExist (Left (DoesNotExist{})) = pure ()
assertDoesNotExist (Left e) = assertFailure $ "Returned: " <> show e

assertRight :: HasCallStack => Either a () -> TestEff ()
assertRight (Left _a) = liftIO $ Test.assertFailure "Test return Left instead of Right"
assertRight (Right b) = pure b

testThis :: String -> TestEff () -> TestEff TestTree
testThis name assertion = do
  let test = runTestEff assertion
  pure $
    Test.testCase name test

testThese :: String -> [TestEff TestTree] -> TestEff TestTree
testThese groupName tests = fmap (Test.testGroup groupName) newTests
  where
    newTests :: TestEff [TestTree]
    newTests = sequenceA tests

assertBool :: Bool -> TestEff ()
assertBool boolean = liftIO $ Test.assertBool "" boolean

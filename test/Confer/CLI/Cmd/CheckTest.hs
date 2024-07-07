{-# LANGUAGE QuasiQuotes #-}

module Confer.CLI.Cmd.CheckTest where

import Data.Function
import Data.List.NonEmpty qualified as NE
import Effectful
import Effectful.Error.Static (Error)
import Effectful.Error.Static qualified as Error
import Effectful.FileSystem (FileSystem)
import Effectful.FileSystem qualified as FileSystem
import System.FilePath qualified as FilePath
import System.IO.Temp qualified as Temporary
import System.OsPath
import System.OsPath qualified as OsPath
import Test.Tasty (TestTree)
import Test.Tasty.HUnit ()

import Confer.CLI.Errors
import Confer.Effect.Symlink (Symlink)
import Confer.Effect.Symlink qualified as Symlink
import Control.Monad (join)
import Utils

spec :: TestEff TestTree
spec =
  testThese
    "Symlink Checks"
    [ testThis "Test that existing symlink succeeds" testExistingSymlink
    , testThis "Test that non-existing symlink fails" testNonExistingSymlink
    , testThis "Test that symlink to non-existing file fails" testSymlinkToNonExistingTarget
    , testThis "Test that symlink to incorrect file fails" testSymlinkToWrongTarget
    , testThis "Test that non-symlink file fails" testNonSymlink
    ]

testExistingSymlink :: TestEff ()
testExistingSymlink =
  Temporary.withSystemTempFile "confer-test.ext" $ \filepath _ ->
    Temporary.withSystemTempDirectory "links.ext" $ \directory -> do
      result <- Error.runErrorNoCallStack . Symlink.runSymlinkIO $ do
        let linkPath = directory <> "/" <> FilePath.takeFileName filepath
        osDestinationPath <- liftIO $ OsPath.encodeFS filepath
        osLinkPath <- liftIO $ OsPath.encodeFS linkPath
        Symlink.createSymlink osDestinationPath osLinkPath
        Symlink.testSymlink osLinkPath osDestinationPath
      case result of
        Left e -> Error.throwError (SymlinkErrors $ NE.singleton e)
        Right _a -> pure ()

testNonExistingSymlink :: TestEff ()
testNonExistingSymlink =
  Temporary.withSystemTempFile "confer-test.ext" $ \filepath _ ->
    Temporary.withSystemTempDirectory "links.ext" $ \directory -> do
      result <- Error.runErrorNoCallStack . Symlink.runSymlinkIO $ do
        osDestinationPath <- liftIO $ OsPath.encodeFS filepath
        let linkPath = directory <> "/" <> FilePath.takeFileName filepath
        osLinkPath <- liftIO $ OsPath.encodeFS linkPath
        Symlink.testSymlink osLinkPath osDestinationPath
      case result of
        Left _ -> pure ()
        Right _ -> assertFailure "This should fail"

testSymlinkToNonExistingTarget :: TestEff ()
testSymlinkToNonExistingTarget =
  Temporary.withSystemTempFile "confer-test.ext" $ \filepath _ ->
    Temporary.withSystemTempDirectory "links.ext" $ \directory -> do
      result <- Error.runErrorNoCallStack . Symlink.runSymlinkIO $ do
        osDestinationPath <- liftIO $ OsPath.encodeFS filepath
        let linkPath = directory <> "/" <> FilePath.takeFileName filepath <> "-wrong"
        osLinkPath <- liftIO $ OsPath.encodeFS linkPath
        Symlink.testSymlink osLinkPath osDestinationPath
      assertDoesNotExist result

testSymlinkToWrongTarget :: TestEff ()
testSymlinkToWrongTarget =
  Temporary.withSystemTempFile "confer-test.ext" $ \filepath _ ->
    Temporary.withSystemTempDirectory "links.ext" $ \directory -> do
      result <- Error.runErrorNoCallStack . Symlink.runSymlinkIO $ do
        osDestinationPath <- liftIO $ OsPath.encodeFS filepath
        let linkPath = directory <> "/" <> FilePath.takeFileName filepath <> "-wrong"
        osLinkPath <- liftIO $ OsPath.encodeFS linkPath
        Symlink.createSymlink osDestinationPath osLinkPath
        Symlink.testSymlink osLinkPath (osDestinationPath <> [osp| -lol |])
      assertWrongTarget result

testNonSymlink :: TestEff ()
testNonSymlink =
  Temporary.withSystemTempFile "confer-test.ext" $ \filepath _ -> do
    result <- Error.runErrorNoCallStack . Symlink.runSymlinkIO $ do
      osFilepath <- liftIO $ OsPath.encodeFS filepath
      Symlink.testSymlink osFilepath [osp| lol |]
    assertIsNotSymlink result

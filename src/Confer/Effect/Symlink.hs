module Confer.Effect.Symlink
  ( createSymlink
  , deleteSymlink
  , testSymlink
  , runSymlinkIO
  , runSymlinkPure
  , Symlink (..)
  , SymlinkError (..)
  ) where

import Control.Exception
import Control.Monad
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static (Error)
import Effectful.Error.Static qualified as Error
import Effectful.FileSystem
import Effectful.FileSystem qualified as FileSystem
import Effectful.State.Static.Local qualified as State
import System.Directory qualified as Directory
import System.Directory.Internal (FileType (..), OsPath)
import System.Directory.Internal qualified as Directory
import System.IO.Error
import System.OsPath qualified as OsPath

data SymlinkError
  = -- | The expected path is not present
    DoesNotExist OsPath
  | IsNotSymlink OsPath
  | -- | The path is unexpectedly present
    AlreadyExists OsPath
  | WrongTarget
      OsPath
      -- ^ Path to the symbolic link
      OsPath
      -- ^ Expected target
      OsPath
      -- ^ Actual target
  deriving stock (Show, Ord, Eq)

data Symlink :: Effect where
  CreateSymlink :: OsPath -> OsPath -> Symlink m ()
  DeleteSymlink :: OsPath -> Symlink m ()
  TestSymlink :: OsPath -> OsPath -> Symlink m (Either SymlinkError ())

type instance DispatchOf Symlink = Dynamic

createSymlink :: Symlink :> es => OsPath -> OsPath -> Eff es ()
createSymlink linkPath destination = send (CreateSymlink linkPath destination)

deleteSymlink :: Symlink :> es => OsPath -> Eff es ()
deleteSymlink target = send (DeleteSymlink target)

testSymlink
  :: Symlink :> es
  => OsPath
  -> OsPath
  -> Eff es (Either SymlinkError ())
testSymlink linkPath expectedLinkTarget =
  send (TestSymlink linkPath expectedLinkTarget)

runSymlinkIO
  :: (IOE :> es, FileSystem :> es, Error SymlinkError :> es)
  => Eff (Symlink : es) a
  -> Eff es a
runSymlinkIO = interpret $ \_ -> \case
  CreateSymlink source destination -> do
    sourceType <- liftIO $ do
      metadata <- Directory.getFileMetadata source
      pure $ Directory.fileTypeFromMetadata metadata
    sourcePath <- FileSystem.makeAbsolute =<< liftIO (OsPath.decodeFS source)
    destinationPath <- liftIO $ OsPath.decodeFS destination
    case sourceType of
      File -> do
        destinationExists <- FileSystem.doesFileExist destinationPath
        if destinationExists
          then handleAlreadyExistingDestination sourcePath destinationPath
          else createFileLink sourcePath destinationPath
      Directory -> do
        destinationExists <- FileSystem.doesDirectoryExist destinationPath
        if destinationExists
          then handleAlreadyExistingDestination sourcePath destinationPath
          else createDirectoryLink sourcePath destinationPath
  DeleteSymlink linkOsPath -> do
    linkFilePath <- liftIO $ OsPath.decodeFS linkOsPath
    sourceType <- liftIO $ do
      metadata <- Directory.getFileMetadata linkOsPath
      pure $ Directory.fileTypeFromMetadata metadata
    case sourceType of
      Directory ->
        FileSystem.removeDirectory linkFilePath
      _ ->
        FileSystem.removeFile linkFilePath
  TestSymlink linkOsPath expectedLinkTarget -> do
    linkFilepath <- liftIO $ OsPath.decodeFS linkOsPath
    liftIO $
      testPath linkFilepath
        `catch` ( \exception -> do
                    if isDoesNotExistError exception
                      then pure $ Left (DoesNotExist linkOsPath)
                      else error (show exception)
                )
    where
      testPath :: FilePath -> IO (Either SymlinkError ())
      testPath linkFilepath = runEff . Error.runErrorNoCallStack . FileSystem.runFileSystem $ do
        isSymbolic <- FileSystem.pathIsSymbolicLink linkFilepath
        unless isSymbolic $
          Error.throwError $
            IsNotSymlink linkOsPath
        actualLinkTarget <- FileSystem.getSymbolicLinkTarget linkFilepath
        expectedLinkTargetFilepath <- liftIO $ OsPath.decodeFS expectedLinkTarget
        actualLinkTargetOsPath <- liftIO $ OsPath.encodeFS actualLinkTarget
        if actualLinkTarget == expectedLinkTargetFilepath
          then pure ()
          else
            Error.throwError $
              WrongTarget
                linkOsPath
                expectedLinkTarget
                actualLinkTargetOsPath

runSymlinkPure
  :: Map OsPath OsPath
  -> Eff (Symlink : es) a
  -> Eff es a
runSymlinkPure virtualFS = reinterpret (State.evalState virtualFS) $ \_ -> \case
  CreateSymlink source destination ->
    State.modify @(Map OsPath OsPath) (Map.insert source destination)
  DeleteSymlink linkPath ->
    State.modify @(Map OsPath OsPath) (Map.delete linkPath)
  TestSymlink linkPath expectedLinkTarget ->
    State.gets @(Map OsPath OsPath) (Map.lookup linkPath) >>= \case
      Just actualLinkTarget ->
        if actualLinkTarget == expectedLinkTarget
          then pure (Right ())
          else
            pure $
              Left
                ( WrongTarget
                    linkPath
                    expectedLinkTarget
                    actualLinkTarget
                )
      Nothing -> pure $ Left (DoesNotExist linkPath)

handleAlreadyExistingDestination
  :: (Error SymlinkError :> es, IOE :> es)
  => FilePath
  -> FilePath
  -> Eff es ()
handleAlreadyExistingDestination sourcePath destinationPath = do
  source <- liftIO $ OsPath.encodeFS sourcePath
  destination <- liftIO $ OsPath.encodeFS destinationPath
  destinationType <- liftIO $ do
    metadata <- Directory.getFileMetadata destination
    pure $ Directory.fileTypeFromMetadata metadata
  destinationIsSymbolic <- liftIO $ Directory.pathIsSymbolicLink destinationPath
  if not destinationIsSymbolic
    then do
      destinationTruePath <-
        liftIO $
          Directory.readSymbolicLink destination
      if destinationTruePath == source
        then do
          liftIO $
            putStrLn $
              destinationPath <> " already points to " <> sourcePath <> "."
        else Error.throwError (AlreadyExists destination)
    else do
      liftIO $ putStrLn $ destinationPath <> " is not a symbolic link but a " <> show destinationType
      Error.throwError (AlreadyExists destination)

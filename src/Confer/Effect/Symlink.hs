module Confer.Effect.Symlink 
  ( createSymlink
  , deleteSymlink
  , testSymlink
  , runSymlinkIO
  , runSymlinkPure
  , verifyExistingSymlink
  , Symlink(..)
  , SymlinkError(..)
  ) where

import Control.Exception
import Control.Monad
import Control.Placeholder
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.FileSystem
import Effectful.FileSystem qualified as FileSystem
import Effectful.State.Static.Local qualified as State
import System.Directory qualified as Directory
import System.Directory.Internal (OsPath, FileType(..))
import System.Directory.Internal qualified as Directory
import System.IO.Error
import System.OsPath qualified as OsPath

data SymlinkError
  = DoesNotExist OsPath
  | IsNotSymlink OsPath
  | WrongTarget 
      OsPath
      -- ^ Path to the symbolic link
      OsPath
      -- ^ Expected target
      OsPath
      -- ^ Actual target
  deriving stock (Show, Eq)

data Symlink :: Effect where
  CreateSymlink :: OsPath -> OsPath -> Symlink m ()
  DeleteSymlink :: OsPath -> Symlink m ()
  TestSymlink :: OsPath -> Symlink m (Either SymlinkError ())

type instance DispatchOf Symlink = Dynamic

createSymlink :: (Symlink :> es) => OsPath -> OsPath -> Eff es ()
createSymlink source destination = send (CreateSymlink source destination)

deleteSymlink :: (Symlink :> es) => OsPath -> Eff es ()
deleteSymlink target = send (DeleteSymlink target)

testSymlink :: (Symlink :> es) => OsPath -> Eff es (Either SymlinkError () )
testSymlink target = send (TestSymlink target)

runSymlinkIO
  :: (IOE :> es, FileSystem :> es)
  => Eff (Symlink : es) a
  -> Eff es a
runSymlinkIO = interpret $ \_ -> \case
  CreateSymlink source destination -> do
    sourceType <- liftIO $ do
      metadata <- Directory.getFileMetadata source
      pure $ Directory.fileTypeFromMetadata metadata
    sourcePath <- liftIO $ OsPath.decodeFS source
    destinationPath <- liftIO $ OsPath.decodeFS destination
    case sourceType of
      File -> 
        createFileLink sourcePath destinationPath
      Directory -> 
        createDirectoryLink sourcePath destinationPath
  DeleteSymlink _ -> todo
  TestSymlink target -> do
    filepath <- liftIO $ OsPath.decodeFS $ target
    isSymbolic <- FileSystem.pathIsSymbolicLink filepath
    liftIO $ catch (testPath isSymbolic) $ \exception -> do
        if isDoesNotExistError exception
        then pure $ Left (DoesNotExist target)
        else pure $ Right ()
    where
      testPath pathIsSymbolic = do
        if pathIsSymbolic 
        then pure $ Right ()
        else pure $ Left (IsNotSymlink target)
    
runSymlinkPure
  :: Map OsPath OsPath
  -> Eff (Symlink : es) a
  -> Eff es a
runSymlinkPure virtualFS = reinterpret (State.evalState virtualFS) $ \_ -> \case
  CreateSymlink source destination -> 
    State.modify @(Map OsPath OsPath) (Map.insert source destination)
  DeleteSymlink linkPath -> 
    State.modify @(Map OsPath OsPath) (Map.delete linkPath)
  TestSymlink linkPath -> State.gets @(Map OsPath OsPath) (Map.lookup linkPath) >>= \case
    Just linkTarget -> pure $ Right ()
    Nothing -> pure $ Left (DoesNotExist linkPath)

verifyExistingSymlink 
  :: (FileSystem :> es) 
  => FilePath 
  -> FilePath
  -> Eff es (Either SymlinkError ())
verifyExistingSymlink linkPath expectedLinkTarget = do
  actualLinkTarget <- FileSystem.getSymbolicLinkTarget linkPath
  if actualLinkTarget == expectedLinkTarget
  then pure (Right ())
  else pure $ Left
    (WrongTarget
      (OsPath.unsafeEncodeUtf linkPath)
      (OsPath.unsafeEncodeUtf expectedLinkTarget)
      (OsPath.unsafeEncodeUtf actualLinkTarget)
    )

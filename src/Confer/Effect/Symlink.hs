module Confer.Effect.Symlink 
  ( createSymlink
  , deleteSymlink
  , testSymlink
  , runSymlinkIO
  , Symlink(..)
  , SymlinkError(..)
  ) where

import Control.Exception
import Control.Monad
import Control.Placeholder
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.FileSystem
import Effectful.FileSystem qualified as FileSystem
import System.Directory qualified as Directory
import System.Directory.Internal (OsPath, FileType(..))
import System.Directory.Internal qualified as Directory
import System.IO.Error
import System.OsPath qualified as OsPath

data SymlinkError
  = DoesNotExist OsPath
  | IsNotSymlink OsPath
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
  TestSymlink target' -> do
    target <- liftIO $ OsPath.decodeFS target'
    liftIO $ catch ( do
      result <- Directory.pathIsSymbolicLink target
      if result 
      then pure $ Right ()
      else pure $ Left (IsNotSymlink target')
      )
      (\exception -> do
        if isDoesNotExistError exception
        then pure $ Left (DoesNotExist target')
        else pure $ Right ()
      )

module Confer.Effect.Symlink 
  ( createSymlink
  , deleteSymlink
  , runSymlinkIO
  , Symlink(..)
  ) where

import Control.Placeholder
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.FileSystem
import System.Directory.Internal
import System.OsPath qualified as OsPath

data Symlink :: Effect where
  CreateSymlink :: OsPath -> OsPath -> Symlink m ()
  DeleteSymlink :: OsPath -> Symlink m ()

type instance DispatchOf Symlink = Dynamic

createSymlink :: (Symlink :> es) => OsPath -> OsPath -> Eff es ()
createSymlink source destination = send (CreateSymlink source destination)

deleteSymlink :: (Symlink :> es) => OsPath -> Eff es ()
deleteSymlink target = send (DeleteSymlink target)

runSymlinkIO
  :: (IOE :> es, FileSystem :> es)
  => Eff (Symlink : es) a
  -> Eff es a
runSymlinkIO = interpret $ \_ -> \case
  CreateSymlink source destination -> do
    sourceType <- liftIO $ do
      metadata <- getFileMetadata source
      pure $ fileTypeFromMetadata metadata
    sourcePath <- liftIO $ OsPath.decodeFS source
    destinationPath <- liftIO $ OsPath.decodeFS destination
    case sourceType of
      File -> 
        createFileLink sourcePath destinationPath
      Directory -> 
        createDirectoryLink sourcePath destinationPath
  DeleteSymlink _ -> todo

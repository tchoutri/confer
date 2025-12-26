module Confer.CLI.Cmd.Deploy (deploy) where

import Control.Monad

import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Text.Display
import Data.Text.IO qualified as Text
import Data.Vector (Vector)
import Effectful
import Effectful.Error.Static
import Effectful.FileSystem (FileSystem)
import Effectful.FileSystem qualified as FileSystem
import System.OsPath (OsPath)
import System.OsPath qualified as OsPath

import Confer.CLI.Errors
import Confer.Config.Types
import Confer.Effect.Symlink (Symlink, SymlinkError (..))
import Confer.Effect.Symlink qualified as Symlink

-- | Take a filtered and checked list of deployments.
--
-- For each fact, we perform sequentially:
--   * Check that the desired file exists
--   * Check that the target symlink does not exist
--      * If it exists, make sure that it points to the
--        file that is version controlled
--          * If it does not, raise an error
deploy
  :: ( FileSystem :> es
     , Symlink :> es
     , IOE :> es
     , Error (NonEmpty CLIError) :> es
     )
  => Bool
  -> Vector Deployment
  -> Eff es ()
deploy quiet deployments = do
  forM_ deployments $ \d ->
    forM_ d.facts $ \fact -> do
      linkFilepath <- liftIO $ OsPath.decodeFS fact.destination
      destinationPathExists <- FileSystem.doesPathExist linkFilepath
      if destinationPathExists
        then do
          result <- Symlink.testSymlink fact.destination fact.source
          case result of
            Left symlinkError -> do
              let cliError = case symlinkError of
                    DoesNotExist path -> symlinkDoesNotExistError path
                    IsNotSymlink path -> pathIsNotSymlinkError path
                    AlreadyExists path -> symlinkAlreadyExistsError path
                    WrongTarget link expected actual -> wrongTargetError link expected actual
              throwError (NE.singleton cliError)
            Right _ ->
              liftIO $ Text.putStrLn $ display (linkFilepath <> " ✅")
        else do
          Symlink.createSymlink fact.source fact.destination
          unless quiet $ do
            liftIO $
              Text.putStrLn $
                "[🔗] " <> display fact

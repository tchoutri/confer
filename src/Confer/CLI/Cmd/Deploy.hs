module Confer.CLI.Cmd.Deploy (deploy) where

import Control.Monad
import Control.Placeholder
import Data.Text.Display
import Data.Text.IO qualified as Text
import Data.Vector (Vector)
import Effectful
import Effectful.FileSystem (FileSystem)
import Effectful.FileSystem qualified as FileSystem
import System.OsPath (OsPath)
import System.OsPath qualified as OsPath

import Confer.Config.Types
import Confer.Effect.Symlink

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
     )
  => Bool
  -> Vector Deployment
  -> Eff es ()
deploy verbose deployments = do
  forM_ deployments $ \d ->
    forM_ d.facts $ \fact -> do
      filepath <- liftIO $ OsPath.decodeFS fact.destination
      destinationPathExists <- FileSystem.doesPathExist filepath
      unless destinationPathExists $ do
        liftIO $
          Text.putStrLn $
            "[🔗] " <> display fact
        createSymlink fact.source fact.destination

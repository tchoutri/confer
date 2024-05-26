module Confer.Cmd.Check (check) where

import Data.Foldable
import Data.Function
import Data.Text qualified as Text
import Data.Text.Display
import Data.Text.IO qualified as Text
import Effectful
import Effectful.FileSystem (FileSystem)
import Effectful.FileSystem qualified as FileSystem
import System.Info qualified as System
import System.OsPath ((</>))
import System.OsPath qualified as OsPath

import Confer.Config.Evaluator
import Confer.Config.Types

check
  :: ( IOE :> es
     , FileSystem :> es
     )
  => Eff es ()
check = do
  loadConfiguration >>= \case
    Right allDeployments -> do
        let currentOS = OS (Text.pack System.os)
        let currentArch = Arch (Text.pack System.arch)
        let deployments = adjustConfiguration currentOS currentArch allDeployments
        forM_ deployments $ \deployment ->
          forM_ deployment.facts $ \fact -> do
            liftIO $ Text.putStrLn $ "[+] Checking " <> display fact
            let osPath = fact.destination </> fact.source
            filePath <- liftIO $ OsPath.decodeFS osPath
            FileSystem.pathIsSymbolicLink filePath
    Left e -> error e

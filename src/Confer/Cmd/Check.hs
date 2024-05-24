module Confer.Cmd.Check (check) where

import Data.Foldable
import Data.Text.Display
import Data.Text.IO qualified as Text
import Effectful
import Effectful.FileSystem (FileSystem)
import Effectful.FileSystem qualified as FileSystem
import System.OsPath ((</>))
import System.OsPath qualified as OsPath

import Confer.Config.Evaluator
import Confer.Config.Types

check :: (IOE :> es, FileSystem :> es) => Eff es ()
check = do
  loadConfiguration >>= \case
    Right deployments -> 
      forM_ deployments $ \deployment -> 
        forM_ deployment.facts $ \fact -> do
          liftIO $ Text.putStrLn $ "[+] Checking " <> display fact
          let osPath = (fact.destination </> fact.source)
          filePath <- (liftIO $ OsPath.decodeFS osPath)
          FileSystem.pathIsSymbolicLink filePath
    Left e -> error e

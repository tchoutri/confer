module Confer.Config.ConfigFile
  ( processConfiguration
  ) where

import Control.Monad (when)
import Control.Placeholder
import Data.Text qualified as Text
import Data.Text.Display
import Data.Text.IO qualified as Text
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Effectful
import Effectful.Error.Static
import Effectful.FileSystem (FileSystem)
import Effectful.FileSystem qualified as FileSystem
import Network.HostName
import System.Info qualified as System
import System.OsPath (OsPath)
import System.OsPath qualified as OsPath

import Confer.CLI.Errors
import Confer.Config.Evaluator
import Confer.Config.Types (Deployment, DeploymentArchitecture (..), DeploymentOS (..))

-- | This function looks up the configuration file in the following places
-- (ordered by position):
--
--  1. Specified on the CLI
--  2. ./deployments.lua
processConfiguration
  :: ( IOE :> es
     , FileSystem :> es
     , Error CLIError :> es
     )
  => Maybe OsPath
  -> Eff es (Vector Deployment)
processConfiguration mConfigurationFilePath = do
  pathToConfigFile <- determineConfigurationFilePath mConfigurationFilePath
  loadConfiguration pathToConfigFile >>= \case
    Right allDeployments -> do
      let currentOS = OS (Text.pack System.os)
      let currentArch = Arch (Text.pack System.arch)
      currentHost <- Text.pack <$> liftIO getHostName
      liftIO $ Text.putStrLn $ "Hostname: " <> currentHost <> " (detected)"
      liftIO $ Text.putStrLn $ "OS: " <> display currentOS <> " (detected)"
      liftIO $ Text.putStrLn $ "Architecture: " <> display currentArch <> " (detected)"
      let deployments =
            adjustConfiguration
              currentHost
              currentOS
              currentArch
              allDeployments
      when (Vector.null deployments) $
        throwError $
          NoDeploymentsAvailable currentOS currentArch currentHost
      pure deployments
    Left e -> error e

determineConfigurationFilePath
  :: (IOE :> es, FileSystem :> es, Error CLIError :> es)
  => Maybe OsPath
  -> Eff es OsPath
determineConfigurationFilePath mCLIConfigFilePath =
  case checkCLIOptions mCLIConfigFilePath of
    Just osPath -> do
      filePath <- liftIO $ OsPath.decodeFS osPath
      FileSystem.doesFileExist filePath
        >>= \case
          False -> throwError $ NoUserProvidedConfigurationFile osPath
          True -> pure osPath
    Nothing ->
      do
        FileSystem.doesFileExist "deployments.lua"
        >>= \case
          False -> throwError NoDefaultConfigurationFile
          True ->
            FileSystem.makeAbsolute "deployments.lua"
              >>= (liftIO . OsPath.encodeFS)

checkCLIOptions :: Maybe OsPath -> Maybe OsPath
checkCLIOptions Nothing = Nothing
checkCLIOptions (Just osPath) = Just osPath

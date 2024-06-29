module Confer.Config.ConfigFile
  ( processConfiguration
  , determineDeploymentOS
  , determineDeploymentArch
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
import Data.Text (Text)

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
  => Bool
  -- ^ Verbose
  -> Maybe OsPath
  -- ^ Potential configuration file path
  -> DeploymentArchitecture
  -- ^ Configured architecture
  -> DeploymentOS
  -- ^ Configured operating system
  -> Maybe Text
  -- ^ hostname override
  -> Eff es (Vector Deployment)
processConfiguration verbose mConfigurationFilePath deploymentArch deploymentOS mHostname = do
  pathToConfigFile <- determineConfigurationFilePath mConfigurationFilePath
  loadConfiguration verbose pathToConfigFile >>= \case
    Right allDeployments -> do
      currentHost <- case mHostname of
        Nothing -> do
          inferredHostname <- Text.pack <$> liftIO getHostName
          when verbose $
            liftIO $
              Text.putStrLn $
                "Hostname: " <> display inferredHostname <> " (detected)"
          pure inferredHostname
        Just overridenHostname -> do
          when verbose $
            liftIO $
              Text.putStrLn $
                "Hostname: " <> display overridenHostname <> " (overriden)"
          pure overridenHostname

      let deployments =
            adjustConfiguration
              currentHost
              deploymentOS
              deploymentArch
              allDeployments
      when (Vector.null deployments) $
        throwError $
          NoDeploymentsAvailable deploymentOS deploymentArch currentHost
      pure deployments
    Left e -> error e

determineConfigurationFilePath
  :: (IOE :> es, FileSystem :> es, Error CLIError :> es)
  => Maybe OsPath
  -> Eff es OsPath
determineConfigurationFilePath mCLIConfigFilePath =
  case mCLIConfigFilePath of
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

determineDeploymentOS
  :: IOE :> es
  => Bool
  -- ^ Verbose mode
  -> Maybe DeploymentOS
  -- Potential override
  -> Eff es DeploymentOS
-- Final result
determineDeploymentOS verbose = \case
  Nothing -> do
    let inferredOS = OS (Text.pack System.os)
    when verbose $ do
      liftIO $ Text.putStrLn $ "OS: " <> display inferredOS <> " (detected)"
    pure inferredOS
  Just overridenOS -> do
    when verbose $
      liftIO $
        Text.putStrLn $
          "OS: " <> display overridenOS <> " (overriden)"
    pure overridenOS

determineDeploymentArch
  :: IOE :> es
  => Bool
  -- ^ Verbose mode
  -> Maybe DeploymentArchitecture
  -- Potential override
  -> Eff es DeploymentArchitecture
-- Final result
determineDeploymentArch verbose = \case
  Nothing -> do
    let inferredArch = Arch (Text.pack System.arch)
    when verbose $ do
      liftIO $ Text.putStrLn $ "Architecture: " <> display inferredArch <> " (detected)"
    pure inferredArch
  Just overridenArch -> do
    when verbose $
      liftIO $
        Text.putStrLn $
          "Architecture: " <> display overridenArch <> " (overriden)"
    pure overridenArch

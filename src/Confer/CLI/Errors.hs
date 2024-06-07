module Confer.CLI.Errors where

import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display
import Data.Text.IO qualified as Text
import System.Exit qualified as System
import System.OsPath (OsPath)
import System.OsPath qualified as OsPath

import Confer.Config.Types
import Confer.Effect.Symlink (SymlinkError (..))
import Confer.Effect.Symlink qualified as Symlink
import Control.Monad.IO.Class (liftIO)
import Data.Foldable

data CLIError
  = NoDefaultConfigurationFile
  | NoUserProvidedConfigurationFile OsPath
  | NoDeploymentsAvailable DeploymentOS DeploymentArchitecture Text
  | SymlinkErrors (NonEmpty SymlinkError)
  deriving stock (Eq, Show)

reportError :: CLIError -> IO ()
reportError NoDefaultConfigurationFile =
  System.die "[!] Could not find configuration file at ./deployments.lua"
reportError (NoUserProvidedConfigurationFile osPath) = do
  filePath <- OsPath.decodeFS osPath
  System.die $ "[!] Could not find configuration file at" <> filePath
reportError (NoDeploymentsAvailable os arch hostname) = do
  let message =
        "[!] Could not find deployments to run on "
          <> display arch
          <> "-"
          <> display os
          <> " "
          <> hostname
  System.die $ Text.unpack message
reportError (SymlinkErrors errors) = do
  forM_ errors $
    \e ->
      liftIO $ Text.putStrLn $ Symlink.formatSymlinkError e
  System.exitFailure

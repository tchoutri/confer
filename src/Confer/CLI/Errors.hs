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
import Data.Word (Word8)

data CLIError
  = NoDefaultConfigurationFile
  | NoUserProvidedConfigurationFile OsPath
  | NoDeploymentsAvailable DeploymentOS DeploymentArchitecture Text
  | SymlinkErrors (NonEmpty SymlinkError)
  deriving stock (Eq, Show)

newtype ErrorCode = ErrorCode Word8
  deriving newtype (Eq, Show, Ord)

instance Display ErrorCode where
  displayBuilder (ErrorCode c) = "[CONFER-" <> displayBuilder c <> "]"

cliErrorToCode :: CLIError -> ErrorCode
cliErrorToCode = \case
  NoDefaultConfigurationFile -> ErrorCode 156
  NoUserProvidedConfigurationFile{} -> ErrorCode 169
  NoDeploymentsAvailable{} -> ErrorCode 123
  SymlinkErrors{} -> ErrorCode 000

symlinkErrorToCode :: SymlinkError -> ErrorCode
symlinkErrorToCode = \case
  DoesNotExist{} -> ErrorCode 234
  AlreadyExists{} -> ErrorCode 205
  IsNotSymlink{} -> ErrorCode 142
  WrongTarget{} -> ErrorCode 102

reportError :: CLIError -> IO ()
reportError NoDefaultConfigurationFile =
  System.die $ Text.unpack $ display (cliErrorToCode NoDefaultConfigurationFile) <> " Could not find configuration file at ./deployments.lua"
reportError e@(NoUserProvidedConfigurationFile osPath) = do
  filePath <- OsPath.decodeFS osPath
  System.die $ Text.unpack $ display (cliErrorToCode e) <> " Could not find configuration file at" <> Text.pack filePath
reportError e@(NoDeploymentsAvailable os arch hostname) = do
  let message =
        display (cliErrorToCode e)
          <> " Could not find deployments to run on "
          <> display arch
          <> "-"
          <> display os
          <> " "
          <> hostname
  System.die $ Text.unpack message
reportError (SymlinkErrors errors) = do
  forM_ errors $
    \err ->
      liftIO $ Text.putStrLn $ formatSymlinkError err
  System.exitFailure

formatSymlinkError :: SymlinkError -> Text
formatSymlinkError e@(DoesNotExist path) =
  display (symlinkErrorToCode e)
    <> " "
    <> display (Text.pack . show $ path)
    <> " does not exist"
formatSymlinkError e@(IsNotSymlink path) =
  display (symlinkErrorToCode e)
    <> " "
    <> display (Text.pack . show $ path)
    <> " is not a symbolic link"
formatSymlinkError e@(AlreadyExists path) =
  display (symlinkErrorToCode e)
    <> " "
    <> display (Text.pack . show $ path)
    <> " already exists"
formatSymlinkError e@(WrongTarget linkPath expectedTarget actualTarget) =
  display (symlinkErrorToCode e)
    <> " "
    <> display (Text.pack . show $ linkPath)
    <> " points to "
    <> display (Text.pack . show $ actualTarget)
    <> " instead of pointing to "
    <> display (Text.pack . show $ expectedTarget)

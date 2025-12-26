module Confer.CLI.Errors
  ( CLIErrorType (..)
  , CLIError
  , noDefaultConfigurationFileError
  , noUserProvidedConfigurationFileError
  , noDeploymentsAvailableError
  , symlinkDoesNotExistError
  , symlinkAlreadyExistsError
  , pathIsNotSymlinkError
  , wrongTargetError
  , reportError
  , toCliError
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Foldable
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display
import Data.Text.IO qualified as Text
import Data.Word (Word8)
import System.Exit qualified as System
import System.OsPath (OsPath)
import System.OsPath qualified as OsPath

import Confer.Config.Types
import Confer.Effect.Symlink (SymlinkError (..))
import Confer.Effect.Symlink qualified as Symlink

data CLIErrorType
  = NoDefaultConfigurationFile
  | NoUserProvidedConfigurationFile
  | NoDeploymentsAvailable
  | SymlinkErrorType SymlinkError
  deriving stock (Eq, Ord, Show)

newtype ErrorCode = ErrorCode Word8
  deriving newtype (Eq, Show, Ord)

instance Display ErrorCode where
  displayBuilder (ErrorCode c) = "[CONFER-" <> displayBuilder c <> "]"

data CLIError = CLIError
  { errorType :: CLIErrorType
  , errorCode :: ErrorCode
  , errorMessage :: Text
  }
  deriving stock (Eq, Ord, Show)

instance Display CLIError where
  displayBuilder cliError = displayBuilder cliError.errorCode <> " " <> displayBuilder cliError.errorMessage

toCliError :: SymlinkError -> CLIError
toCliError = \case
  DoesNotExist path -> symlinkDoesNotExistError path
  IsNotSymlink path -> pathIsNotSymlinkError path
  AlreadyExists path -> symlinkAlreadyExistsError path
  WrongTarget link expected actual -> wrongTargetError link expected actual

noDefaultConfigurationFileError :: CLIError
noDefaultConfigurationFileError =
  CLIError
    { errorType = NoDefaultConfigurationFile
    , errorCode = ErrorCode 156
    , errorMessage = "Could not find configuration file at ./deployments.lua"
    }

noUserProvidedConfigurationFileError :: OsPath -> CLIError
noUserProvidedConfigurationFileError path =
  CLIError
    { errorType = NoUserProvidedConfigurationFile
    , errorCode = ErrorCode 169
    , errorMessage = "Could not find configuration file at" <> Text.show path
    }

noDeploymentsAvailableError :: DeploymentOS -> DeploymentArchitecture -> Text -> CLIError
noDeploymentsAvailableError os arch hostname =
  CLIError
    { errorType = NoDeploymentsAvailable
    , errorCode = ErrorCode 123
    , errorMessage =
        " Could not find deployments to run on "
          <> display arch
          <> "-"
          <> display os
          <> " "
          <> hostname
    }

symlinkDoesNotExistError :: OsPath -> CLIError
symlinkDoesNotExistError path =
  CLIError
    { errorType = SymlinkErrorType (DoesNotExist path)
    , errorCode = ErrorCode 234
    , errorMessage = Text.show path <> " does not exist"
    }

symlinkAlreadyExistsError :: OsPath -> CLIError
symlinkAlreadyExistsError path =
  CLIError
    { errorType = SymlinkErrorType (AlreadyExists path)
    , errorCode = ErrorCode 205
    , errorMessage = Text.show path <> " already exists"
    }

pathIsNotSymlinkError :: OsPath -> CLIError
pathIsNotSymlinkError path =
  CLIError
    { errorType = SymlinkErrorType (IsNotSymlink path)
    , errorCode = ErrorCode 142
    , errorMessage = Text.show path <> " is not a symbolic link"
    }

wrongTargetError
  :: OsPath
  -- ^ link path
  -> OsPath
  -- ^  expected target
  -> OsPath
  -- ^ actual target
  -> CLIError
wrongTargetError linkPath expectedTarget actualTarget =
  CLIError
    { errorType = SymlinkErrorType (WrongTarget linkPath expectedTarget actualTarget)
    , errorCode = ErrorCode 102
    , errorMessage =
        display (Text.show linkPath)
          <> " points to "
          <> display (Text.show actualTarget)
          <> " instead of pointing to "
          <> display (Text.show expectedTarget)
    }

reportError :: CLIError -> IO ()
reportError cliError =
  Text.putStrLn $ display cliError

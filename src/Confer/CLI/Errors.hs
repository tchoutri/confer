module Confer.CLI.Errors where

import System.OsPath (OsPath)

data CLIError
  = NoDefaultConfigurationFile
  | NoUserProvidedConfigurationFile OsPath
  deriving stock (Eq, Show)

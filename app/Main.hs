{-# LANGUAGE QuasiQuotes #-}

module Main where

import Control.Placeholder
import Data.Function ((&))
import Data.Map.Strict qualified as Map
import Effectful
import Effectful.Error.Static
import Effectful.FileSystem
import Options.Applicative
import Options.Applicative.Types
import System.Exit qualified as System
import System.OsPath
import System.OsPath qualified as OsPath

import Confer.CLI.Cmd.Check qualified as Cmd
import Confer.CLI.Cmd.Deploy qualified as Cmd
import Confer.CLI.Errors
import Confer.Config.ConfigFile
import Confer.Config.Evaluator
import Confer.Effect.Symlink

data Options = Options
  { cliCommand :: Command
  }
  deriving stock (Show, Eq)

data Command
  = Check CmdOptions
  | Deploy CmdOptions
  deriving stock (Show, Eq)

data CmdOptions = CmdOptions
  { dryRun :: Bool
  , configurationFile :: Maybe OsPath
  }
  deriving stock (Show, Eq)

main :: IO ()
main = do
  parseResult <- execParser (parseOptions `withInfo` "confer – The dotfiles manager")
  result <-
    runOptions parseResult
      & runFileSystem
      & runErrorNoCallStack
      & runEff
  case result of
    Right _ -> pure ()
    Left NoDefaultConfigurationFile ->
      liftIO $ System.die "Could not find configuration file at ./deployments.lua"
    Left (NoUserProvidedConfigurationFile osPath) -> do
      filePath <- liftIO $ OsPath.decodeFS osPath
      liftIO $ System.die $ "Could not find configuration file at" <> filePath

parseOptions :: Parser Options
parseOptions =
  Options <$> parseCommand

parseCommand :: Parser Command
parseCommand =
  subparser $
    command "check" (parseCheck `withInfo` "Ensure that the configured link destinations do not exist as files already")
      <> command "deploy" (parseDeploy `withInfo` "Deploy the configured symbolic links")

parseCheck :: Parser Command
parseCheck =
  Check
    <$> ( CmdOptions
            <$> switch (long "dry-run" <> help "Do not perform actual file system operations")
            <*> optional (option osPathOption (long "deployments-file" <> metavar "FILENAME" <> help "Use the specified the deployments.lua file"))
        )

parseDeploy :: Parser Command
parseDeploy =
  Deploy
    <$> ( CmdOptions
            <$> switch (long "dry-run" <> help "Do not perform actual file system operations")
            <*> optional (option osPathOption (long "deployments-file" <> metavar "FILENAME" <> help "Use the specified the deployments.lua file"))
        )

runOptions
  :: ( IOE :> es
     , Error CLIError :> es
     , FileSystem :> es
     )
  => Options
  -> Eff es ()
runOptions (Options (Check cmdOptions)) = do
  deployments <- processConfiguration cmdOptions.configurationFile
  if cmdOptions.dryRun
    then
      Cmd.check deployments
        & runSymlinkPure Map.empty
    else
      Cmd.check deployments
        & runSymlinkIO
runOptions (Options (Deploy cmdOptions)) = do
  deployments <- processConfiguration cmdOptions.configurationFile
  if cmdOptions.dryRun
    then
      Cmd.deploy deployments
        & runSymlinkPure Map.empty
    else
      Cmd.deploy deployments
        & runSymlinkIO

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

osPathOption :: ReadM OsPath
osPathOption = maybeReader (OsPath.encodeUtf)

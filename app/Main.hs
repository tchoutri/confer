module Main where

import Data.Function ((&))
import Data.Map.Strict qualified as Map
import Data.Version (showVersion)
import Effectful
import Effectful.Error.Static
import Effectful.FileSystem
import Options.Applicative
import Options.Applicative.Types
import Paths_confer (version)
import System.IO
import System.OsPath
import System.OsPath qualified as OsPath

import Confer.CLI.Cmd.Check qualified as Cmd
import Confer.CLI.Cmd.Deploy qualified as Cmd
import Confer.CLI.Errors
import Confer.Config.ConfigFile
import Confer.Config.Evaluator
import Confer.Effect.Symlink

data Options = Options
  { dryRun :: Bool
  , verbose :: Bool
  , configurationFile :: Maybe OsPath
  , cliCommand :: Command
  }
  deriving stock (Show, Eq)

data Command
  = Check
  | Deploy
  deriving stock (Show, Eq)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  parseResult <- execParser (parseOptions `withInfo` "confer – The dotfiles manager")
  result <-
    runOptions parseResult
      & runFileSystem
      & runErrorNoCallStack
      & runEff
  case result of
    Right _ -> pure ()
    Left e -> reportError e

parseOptions :: Parser Options
parseOptions =
  Options
    <$> switch
      (long "dry-run" <> help "Do not perform actual file system operations")
    <*> switch (long "verbose" <> help "Make the program more talkative")
    <*> optional (option osPathOption (long "deployments-file" <> metavar "FILENAME" <> help "Use the specified deployments.lua file"))
    <*> parseCommand

parseCommand :: Parser Command
parseCommand =
  subparser $
    command "check" (parseCheck `withInfo` "Perform sanity checks on the link destinations")
      <> command "deploy" (parseDeploy `withInfo` "Deploy the configured symbolic links")

parseCheck :: Parser Command
parseCheck = pure Check

parseDeploy :: Parser Command
parseDeploy = pure Deploy

runOptions
  :: ( IOE :> es
     , Error CLIError :> es
     , FileSystem :> es
     )
  => Options
  -> Eff es ()
runOptions (Options dryRun verbose configurationFile Check) = do
  deployments <- processConfiguration verbose configurationFile
  if dryRun
    then
      Cmd.check verbose deployments
        & runSymlinkPure Map.empty
    else
      Cmd.check verbose deployments
        & runSymlinkIO
runOptions (Options dryRun verbose configurationFile Deploy) = do
  deployments <- processConfiguration verbose configurationFile
  if dryRun
    then
      Cmd.deploy verbose deployments
        & runSymlinkPure Map.empty
    else
      Cmd.deploy verbose deployments
        & runSymlinkIO

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc =
  info
    ( simpleVersioner (showVersion version)
        <*> helper
        <*> opts
    )
    $ progDesc desc

osPathOption :: ReadM OsPath
osPathOption = maybeReader OsPath.encodeUtf

{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.Function ((&))
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Version (showVersion)
import Development.GitRev
import Effectful
import Effectful.Error.Static
import Effectful.Error.Static qualified as Error
import Effectful.FileSystem
import Options.Applicative
import Options.Applicative.Help.Pretty
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
import Confer.Config.Types
import Confer.Effect.Symlink

data Options = Options
  { dryRun :: Bool
  , verbose :: Bool
  , configurationFile :: Maybe OsPath
  , mDeploymentArch :: Maybe DeploymentArchitecture
  , mDeploymentOs :: Maybe DeploymentOS
  , mDeploymentHostname :: Maybe Text
  , pretty :: Bool
  , cliCommand :: Command
  }
  deriving stock (Show, Eq)

data Command
  = Check
  | Deploy
  deriving stock (Show, Eq)

programVersion :: String
programVersion =
  "confer version " <> showVersion version <> " (commit " <> $(gitHash) <> ")"

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  let opts =
        info (parseOptions <**> simpleVersioner programVersion <**> helper) $
          header "confer – The dotfiles manager"
            <> progDescDoc (Just programDescription)
            <> footerDoc (Just programFooter)
  parseResult <- execParser opts
  result <-
    runOptions parseResult
      & runFileSystem
      & runErrorNoCallStack
      & runEff
  case result of
    Right _ -> pure ()
    Left e -> reportError e

programDescription :: Doc
programDescription = "Confer handles the deployment and synchronisation of your configuration files."

programFooter :: Doc
programFooter =
  vsep
    [ "Git repository: https://github.com/tchoutri/confer"
    ]

parseOptions :: Parser Options
parseOptions =
  Options
    <$> switch
      (long "dry-run" <> help "Do not perform actual file system operations")
    <*> switch (long "verbose" <> help "Make the program more talkative")
    <*> optional
      (option osPathOption (long "deployments-file" <> metavar "FILE" <> help "Use the specified deployments.lua file"))
    <*> optional
      (option deploymentArchOption (long "arch" <> metavar "ARCH" <> help "Override the detected architecture"))
    <*> optional
      (option deploymentOsOption (long "os" <> metavar "OS" <> help "Override the detected operating system "))
    <*> optional
      (option str (long "hostname" <> metavar "HOSTNAME" <> help "Override the detected host name"))
    <*> switch (long "pretty" <> help "Enable pretty output on the console")
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
runOptions (Options dryRun verbose configurationFile mArch mOs mHostname isPretty Check) = do
  deploymentArch <- determineDeploymentArch verbose mArch
  deploymentOS <- determineDeploymentOS verbose mOs
  deployments <-
    processConfiguration
      verbose
      configurationFile
      deploymentArch
      deploymentOS
      mHostname
  if dryRun
    then
      Cmd.check verbose deployments
        & runSymlinkPure Map.empty
    else do
      result <-
        Cmd.check verbose deployments
          & runSymlinkIO
          & runErrorNoCallStack
      case result of
        Left symlinkError -> Error.throwError (SymlinkErrors (NE.singleton symlinkError))
        Right a -> pure a
runOptions (Options dryRun verbose configurationFile mArch mOs mHostname isPretty Deploy) = do
  deploymentArch <- determineDeploymentArch verbose mArch
  deploymentOS <- determineDeploymentOS verbose mOs
  deployments <- processConfiguration verbose configurationFile deploymentArch deploymentOS mHostname
  if dryRun
    then
      Cmd.deploy verbose deployments
        & runSymlinkPure Map.empty
    else do
      result <-
        Cmd.deploy verbose deployments
          & runSymlinkIO
          & runErrorNoCallStack
      case result of
        Left symlinkError -> Error.throwError (SymlinkErrors (NE.singleton symlinkError))
        Right a -> pure a

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

deploymentOsOption :: ReadM DeploymentOS
deploymentOsOption = maybeReader $
  \case
    "all" -> Just AllOS
    os -> Just $ OS (Text.pack os)

deploymentArchOption :: ReadM DeploymentArchitecture
deploymentArchOption = maybeReader $
  \case
    "all" -> Just AllArchs
    arch -> Just $ Arch (Text.pack arch)

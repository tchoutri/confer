{-# LANGUAGE QuasiQuotes #-}
module Main where

import Data.Function ((&))
import Data.Map.Strict qualified as Map
import Effectful
import Effectful.FileSystem
import Options.Applicative
import System.OsPath

import Confer.Cmd.Check qualified as Cmd
import Confer.Cmd.Deploy qualified as Cmd
import Confer.Config.Evaluator
import Confer.Effect.Symlink 

data Options = Options
  { cliCommand :: Command
  }
  deriving stock (Show, Eq)

data Command
  = Check
  | Deploy DeployOptions
  deriving stock (Show, Eq)

data DeployOptions = DeployOptions
  { dryRun :: Bool
  }
  deriving stock (Show, Eq)

main :: IO ()
main = do
  parseResult <- execParser (parseOptions `withInfo` "confer – The dotfiles manager")
  runOptions parseResult
    & runFileSystem
    & runEff

parseOptions :: Parser Options
parseOptions =
  Options <$> parseCommand

parseCommand :: Parser Command
parseCommand =
  subparser $
    command "check" (parseCheck `withInfo` "Ensure that the configured link destinations do not exist as files already")
    <> command "deploy" (parseDeploy `withInfo` "Deploy the configured symbolic links")

parseCheck :: Parser Command
parseCheck = pure Check 

parseDeploy :: Parser Command 
parseDeploy =
  Deploy <$>
    ( DeployOptions 
      <$> switch (long "dry-run" <> help "Do not perform actual file system operations")
    )

runOptions
  :: ( IOE :> es
     , FileSystem :> es
     )
  => Options
  -> Eff es ()
runOptions (Options Check) = do
  deployments <- processConfiguration [osp|doc/confer_example.lua|]
  Cmd.check deployments
    & runSymlinkIO 
runOptions (Options (Deploy deployOptions)) = do
  deployments <- processConfiguration [osp|doc/confer_example.lua|]
  if deployOptions.dryRun
  then
    Cmd.deploy deployments
      & runSymlinkPure Map.empty
  else
    Cmd.deploy deployments
      & runSymlinkIO

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

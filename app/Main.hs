module Main where

import Effectful
import Data.Function ((&))
import Effectful.FileSystem
import Options.Applicative

import Confer.Effect.Symlink 
import Confer.Cmd.Check (check)

data Options = Options
  { cliCommand :: Command
  }
  deriving stock (Show, Eq)

data Command
  = Check
  deriving stock (Show, Eq)

main :: IO ()
main = do
  parseResult <- execParser (parseOptions `withInfo` "confer – The dotfiles manager")
  runOptions parseResult
    & runEff

parseOptions :: Parser Options
parseOptions =
  Options <$> parseCommand

parseCommand :: Parser Command
parseCommand =
  subparser $
    command "check" (parseCheck `withInfo` "Ensure that the configured link destinations do not exist as files already")

parseCheck :: Parser Command
parseCheck = pure Check 

runOptions
  :: ( IOE :> es
     )
  => Options
  -> Eff es ()
runOptions (Options Check) = 
  check
    & runSymlinkIO 
    & runFileSystem

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

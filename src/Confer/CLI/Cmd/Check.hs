module Confer.CLI.Cmd.Check (check) where

import Control.Monad
import Control.Placeholder
import Data.Foldable
import Data.Function
import Data.List.NonEmpty
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display
import Data.Text.IO qualified as Text
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Effectful
import Effectful.Error.Static (Error)
import Effectful.Error.Static qualified as Error
import Effectful.FileSystem (FileSystem)
import Effectful.FileSystem qualified as FileSystem
import System.Exit qualified as System
import System.Info qualified as System
import System.OsPath (OsPath)
import System.OsPath qualified as OsPath
import Validation

import Confer.CLI.Errors (CLIError (..))
import Confer.CLI.Errors qualified as Errors
import Confer.Config.Evaluator
import Confer.Config.Types
import Confer.Effect.Symlink (Symlink, SymlinkError (..))
import Confer.Effect.Symlink qualified as Symlink

check
  :: ( IOE :> es
     , Symlink :> es
     , Error CLIError :> es
     )
  => Vector Deployment
  -> Eff es ()
check deployments = do
  result <-
    mconcat . Vector.toList <$> do
      let facts :: Vector Fact = foldMap (.facts) deployments
      forM facts $ \fact -> do
        liftIO $ Text.putStrLn $ "[+] Checking " <> display fact
        validateSymlink fact
  case result of
    Failure errors -> do
      Error.throwError (SymlinkErrors errors)
    Success _ -> pure ()

validateSymlink
  :: Symlink :> es
  => Fact
  -> Eff es (Validation (NonEmpty SymlinkError) ())
validateSymlink fact = do
  result <- Symlink.testSymlink fact.destination fact.source
  case result of
    Right _ -> pure $ Success ()
    Left e -> pure $ Failure (NE.singleton e)

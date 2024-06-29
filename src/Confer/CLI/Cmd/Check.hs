module Confer.CLI.Cmd.Check (check) where

import Control.Monad
import Data.List.NonEmpty
import Data.List.NonEmpty qualified as NE
import Data.Text.Display
import Data.Text.IO qualified as Text
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Effectful
import Effectful.Error.Static (Error)
import Effectful.Error.Static qualified as Error
import Validation

import Confer.CLI.Errors (CLIError (..))
import Confer.Config.Types
import Confer.Effect.Symlink (Symlink, SymlinkError (..))
import Confer.Effect.Symlink qualified as Symlink

check
  :: ( IOE :> es
     , Symlink :> es
     , Error CLIError :> es
     )
  => Bool
  -> Vector Deployment
  -> Eff es ()
check verbose deployments = do
  result <-
    mconcat . Vector.toList <$> do
      let facts :: Vector Fact = foldMap (.facts) deployments
      forM facts $ \fact -> do
        liftIO $ Text.putStrLn $ "Checking " <> display fact
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

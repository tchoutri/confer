module Confer.CLI.Cmd.Check (check) where

import Control.Monad
import Data.List.NonEmpty
import Data.List.NonEmpty qualified as NE
import Data.Text.Display
import Data.Text.IO qualified as Text
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Effectful
import Effectful.Concurrent
import Effectful.Console.ByteString
import Effectful.Error.Static (Error)
import Effectful.Error.Static qualified as Error
import GHC.Float
import Validation

import Confer.CLI.Errors (CLIError (..))
import Confer.CLI.UI
import Confer.Config.Types
import Confer.Effect.Symlink (Symlink, SymlinkError (..))
import Confer.Effect.Symlink qualified as Symlink

check
  :: ( Symlink :> es
     , Error CLIError :> es
     , Console :> es
     , Concurrent :> es
     )
  => Bool
  -> Vector Deployment
  -> Eff es ()
check quiet deployments = do
  result <-
    mconcat . Vector.toList <$> do
      let facts :: Vector Fact = foldMap (.facts) deployments
      Vector.iforM facts $ \index fact -> do
        let percentage = (int2Double index / (int2Double $ Vector.length facts))
        threadDelay 30_000
        unless quiet $ printProgress "Checking links" percentage
        validateSymlink fact
  case result of
    Failure errors -> do
      Error.throwError (SymlinkErrors errors)
    Success _ -> do
      unless quiet $ printProgress "Checking links" 1.0
      pure ()

validateSymlink
  :: Symlink :> es
  => Fact
  -> Eff es (Validation (NonEmpty SymlinkError) ())
validateSymlink fact = do
  result <- Symlink.testSymlink fact.destination fact.source
  case result of
    Right _ -> pure $ Success ()
    Left e -> pure $ Failure (NE.singleton e)

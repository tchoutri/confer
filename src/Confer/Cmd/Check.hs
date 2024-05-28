module Confer.Cmd.Check (check) where

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
import Effectful.FileSystem (FileSystem)
import Effectful.FileSystem qualified as FileSystem
import System.Exit qualified as System
import System.Info qualified as System
import System.OsPath ((</>), OsPath)
import System.OsPath qualified as OsPath
import Validation

import Confer.Config.Evaluator
import Confer.Config.Types
import Confer.Effect.Symlink (Symlink, SymlinkError(..))
import Confer.Effect.Symlink qualified as Symlink

check
  :: ( IOE :> es
     , Symlink :> es
     )
  => Vector Deployment
  -> Eff es ()
check deployments = do
  result <- mconcat . Vector.toList <$> do
    let facts :: Vector Fact = foldMap (.facts) deployments
    forM facts $ \fact -> do
      liftIO $ Text.putStrLn $ "[+] Checking " <> display fact
      validateSymlink fact
  case result of
    Failure errors -> do
      forM_ errors $
        \e ->
          liftIO $ Text.putStrLn $ formatSymlinkError e
      liftIO System.exitFailure
    Success _ -> pure ()


validateSymlink
  :: (Symlink :> es)
  => Fact
  -> Eff es (Validation (NonEmpty SymlinkError) ())
validateSymlink fact = do
  let osPath = fact.destination </> fact.source
  result <- Symlink.testSymlink osPath
  case result of
    Right _ -> pure $ Success ()
    Left e -> pure $ Failure (NE.singleton e)

formatSymlinkError :: SymlinkError -> Text
formatSymlinkError (DoesNotExist path) =
  "[!] "
    <> display (Text.pack . show $ path)
    <> " does not exist"
formatSymlinkError (IsNotSymlink path) =
  "[!] "
    <> display (Text.pack . show $ path)
    <> " is not a symbolic link"
formatSymlinkError (WrongTarget linkPath expectedTarget actualTarget) =
  "[!] "
    <> display (Text.pack . show $ linkPath)
    <> " points to "
    <> display (Text.pack . show $ actualTarget)
    <> " instead of pointing to "
    <> display (Text.pack . show $ expectedTarget)

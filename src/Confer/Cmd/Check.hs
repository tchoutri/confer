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
     , FileSystem :> es
     , Symlink :> es
     )
  => Eff es ()
check = do
  loadConfiguration >>= \case
    Right allDeployments -> do
        let currentOS = OS (Text.pack System.os)
        let currentArch = Arch (Text.pack System.arch)
        let deployments = adjustConfiguration currentOS currentArch allDeployments
        result <- sequenceA . join <$> mapM (\deployment ->
          forM deployment.facts $ \fact -> do
            liftIO $ Text.putStrLn $ "[+] Checking " <> display fact
            validateSymlink fact) deployments
        case result of
          Failure errors -> do
            forM_ errors $
              \e ->
                liftIO $ Text.putStrLn $ formatSymlinkError e
            liftIO System.exitFailure
          Success _ -> pure ()
    Left e -> error e

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

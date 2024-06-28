module Confer.Config.Evaluator
  ( loadConfiguration
  , adjustConfiguration
  ) where

import Control.Monad (void)
import Control.Placeholder
import Data.Maybe (isNothing)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Debug.Trace
import Effectful
import Effectful.FileSystem (FileSystem)
import HsLua.Core (Exception)
import HsLua.Core qualified as Lua
import HsLua.Marshalling (Peeker, Result)
import HsLua.Marshalling qualified as Lua
import HsLua.Module.System qualified as Lua.System
import HsLua.Packaging.Module qualified as Lua
import Paths_confer (getDataFileName)
import System.Directory qualified as Directory
import System.IO (utf16le, utf8)
import System.IO.Unsafe qualified as Unsafe
import System.Info qualified as System
import System.OsPath (OsPath)
import System.OsPath qualified as OsPath
import System.OsPath.Encoding qualified as OsPath

import Confer.API.Host qualified as API
import Confer.API.User qualified as API
import Confer.Config.Types

adjustConfiguration
  :: Text
  -> DeploymentOS
  -> DeploymentArchitecture
  -> Vector Deployment
  -> Vector Deployment
adjustConfiguration hostname os arch deployments =
  Vector.filter
    ( \d ->
        (d.os == AllOS || d.os == os)
          && (d.architecture == AllArchs || d.architecture == arch)
          && (d.hostname == Just hostname || isNothing d.hostname)
    )
    deployments

loadConfiguration
  :: ( IOE :> es
     , FileSystem :> es
     )
  => OsPath
  -> Eff es (Either String (Vector Deployment))
loadConfiguration pathToConfigFile = do
  userModule <- API.mkUserModule
  hostModule <- API.mkHostModule
  liftIO $ Lua.run $ do
    Lua.openlibs -- load the default Lua packages
    conferLuaFilePath <- liftIO $ getDataFileName "runtime/lua/confer.lua"
    liftIO $ Text.putStrLn $ "Loading " <> Text.pack conferLuaFilePath
    Lua.dofile (Just conferLuaFilePath)
    Lua.setglobal "confer"
    Lua.registerModule Lua.System.documentedModule
    Lua.registerModule userModule
    Lua.registerModule hostModule
    configFilePath <- liftIO $ OsPath.decodeFS pathToConfigFile
    liftIO $ Text.putStrLn $ "Loading " <> Text.pack (show configFilePath)
    Lua.dofile (Just configFilePath)
      >>= \case Lua.OK -> pure (); _ -> Lua.throwErrorAsException
    Lua.resultToEither <$> Lua.runPeeker peekConfig Lua.top

peekConfig :: Peeker Exception (Vector Deployment)
peekConfig index =
  Lua.retrieving "config" $
    Vector.fromList <$> Lua.peekList peekDeployment index

peekDeployment :: Peeker Exception Deployment
peekDeployment index = Lua.retrieving "deployment" $ do
  hostname <- Lua.peekFieldRaw (Lua.peekNilOr Lua.peekText) "hostname" index
  architecture <-
    maybeToDeploymentArchitecture
      <$> Lua.peekFieldRaw (Lua.peekNilOr Lua.peekText) "architecture" index
  os <-
    maybeToDeploymentOS
      <$> Lua.peekFieldRaw (Lua.peekNilOr Lua.peekText) "os" index
  facts <- Vector.fromList <$> Lua.peekFieldRaw (Lua.peekList peekFact) "facts" index
  let deployment = Deployment{..}
  pure deployment

peekFact :: Peeker Exception Fact
peekFact index = Lua.retrieving "fact" $ do
  name <- Lua.peekFieldRaw Lua.peekText "name" index
  source <- Lua.peekFieldRaw peekOsPath "source" index
  destination <- Lua.peekFieldRaw peekOsPath "destination" index
  let fact = Fact{..}
  pure fact

peekOsPath :: Peeker Exception OsPath
peekOsPath index = do
  result <- Lua.peekString index
  let absolutePath = Unsafe.unsafePerformIO $ Directory.makeAbsolute result
  case OsPath.encodeWith utf8 utf16le absolutePath of
    Right p -> pure p
    Left e -> fail $ OsPath.showEncodingException e

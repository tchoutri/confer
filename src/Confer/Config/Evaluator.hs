module Confer.Config.Evaluator ( loadConfiguration ) where

import Control.Monad (void)
import Control.Placeholder
import Data.Text qualified as Text
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Debug.Trace
import Effectful
import Effectful.FileSystem (FileSystem)
import HsLua.Core (Exception)
import HsLua.Core qualified as Lua
import HsLua.Marshalling (Result, Peeker)
import HsLua.Marshalling qualified as Lua
import HsLua.Module.System qualified as Lua.System
import HsLua.Packaging.Module qualified as Lua
import System.IO (utf8, utf16le)
import System.OsPath (OsPath)
import System.OsPath qualified as OsPath
import System.OsPath.Encoding qualified as OsPath

import Confer.Config.Types
import Confer.API.Host qualified as API
import Confer.API.User qualified as API

loadConfiguration 
  :: ( IOE :> es
     , FileSystem :> es
     )
  => Eff es (Either String (Vector Deployment))
loadConfiguration = do
  userModule <- API.mkUserModule
  hostModule <- API.mkHostModule
  liftIO $ Lua.run $ do
    Lua.openlibs -- load the default Lua packages
    Lua.dofile (Just "./runtime/lua/confer.lua")
    Lua.registerModule Lua.System.documentedModule
    Lua.registerModule userModule
    Lua.registerModule hostModule
    Lua.dofile (Just "./doc/confer_example.lua")
    Lua.resultToEither <$> Lua.runPeeker peekConfig Lua.top

peekConfig :: Peeker Exception (Vector Deployment)
peekConfig index = Lua.retrieving "config" $
  Vector.fromList <$> Lua.peekList peekDeployment index

peekDeployment :: Peeker Exception Deployment
peekDeployment index = Lua.retrieving "deployment" $ do
  hostname <- Lua.retrieving "deployment.hostname" $
    Lua.peekFieldRaw (Lua.peekNilOr Lua.peekText) "hostname" index
  architecture <- Lua.retrieving "deployment.architecture" $
    Lua.peekFieldRaw (Lua.peekNilOr Lua.peekText) "architecture" index
  os <- Lua.retrieving "deployment.os" $
    Lua.peekFieldRaw (Lua.peekNilOr Lua.peekText) "os" index
  facts <- Lua.retrieving "deployment.facts" $
    Vector.fromList <$> Lua.peekFieldRaw (Lua.peekList peekFact) "facts" index
  let deployment = Deployment{..}
  pure deployment

peekFact :: Peeker Exception Fact
peekFact index = Lua.retrieving "fact" $ do
  name <- Lua.retrieving "fact.name" $
    Lua.peekFieldRaw Lua.peekText "name" index
  source <- Lua.retrieving "fact.source" $
    Lua.peekFieldRaw peekOsPath "source" index
  destination <- Lua.retrieving "fact.destination" $
    Lua.peekFieldRaw peekOsPath "destination" index
  let fact = Fact{..}
  pure fact

peekOsPath :: Peeker Exception OsPath
peekOsPath index = do
  result <- Lua.peekText index
  case OsPath.encodeWith utf8 utf16le (Text.unpack result) of
    Right p -> pure p
    Left e -> fail $ OsPath.showEncodingException e

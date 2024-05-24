module Confer.Config.Evaluator ( loadConfiguration ) where

import Control.Monad (void)
import Control.Placeholder
import Data.Text qualified as Text
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Debug.Trace
import Effectful
import HsLua.Core (Exception)
import HsLua.Core qualified as Lua
import HsLua.Marshalling (Result, Peeker)
import HsLua.Marshalling qualified as Lua
import System.IO (utf8)
import System.OsPath (OsPath)
import System.OsPath qualified as OsPath
import System.OsPath.Encoding qualified as OsPath

import Confer.Config.Types

loadConfiguration :: (IOE :> es) => Eff es (Result (Vector Deployment))
loadConfiguration= liftIO $ Lua.run $ do
  Lua.openlibs -- load the default Lua packages
  apiLoadStatus <- Lua.dofile (Just "./lua/runtime/confer.lua")  -- load and run the program
  liftIO $ putStrLn $ "[+] API loaded: " <> show apiLoadStatus
  Lua.setglobal "confer"
  configLoadStatus <- Lua.dofile (Just "./doc/confer_example.lua")  -- load and run the program
  liftIO $ putStrLn $ "[+] Configuration file loaded: " <> show configLoadStatus
  Lua.runPeeker peekConfig Lua.top

peekConfig :: Peeker Exception (Vector Deployment)
peekConfig index = Lua.retrieving "config" $ do
  result <- Vector.fromList <$> Lua.peekList peekDeployment index
  traceShowM $ "[+] Configuration " <> show result
  pure result

peekDeployment :: Peeker Exception Deployment
peekDeployment index = Lua.retrieving "deployment" $ do
  hostname <- traceShowId <$> Lua.peekFieldRaw (Lua.peekNilOr Lua.peekText) "hostname" index
  architecture <- traceShowId <$> Lua.peekFieldRaw (Lua.peekNilOr Lua.peekText) "architecture" index
  os <- traceShowId <$> Lua.peekFieldRaw (Lua.peekNilOr Lua.peekText) "os" index
  facts <- traceShowId . Vector.fromList <$> Lua.peekFieldRaw (Lua.peekList peekFact) "facts" index
  threadStatus <- Lua.liftLua $ Lua.status @Exception
  let deployment = Deployment{..}
  traceShowM $ "[+] peekDeployment status: " <> show threadStatus
  traceShowM $ "[+] Deployment " <> show deployment
  pure deployment

peekFact :: Peeker Exception Fact
peekFact index = Lua.retrieving "fact" $ do
  name <- Lua.peekFieldRaw (Lua.peekText) "name" index
  source <- Lua.peekFieldRaw (peekOsPath) "source" index
  destination <- Lua.peekFieldRaw (peekOsPath) "destination" index
  threadStatus <- Lua.liftLua $ Lua.status @Exception
  let fact = Fact{..}
  traceShowM $ "[+] peekFact status: " <> show threadStatus
  traceShowM $ "[+] Fact " <> show fact
  pure fact
  
peekOsPath :: Peeker Exception OsPath
peekOsPath index = do
  result <- Lua.peekText index
  case OsPath.encodeWith utf8 utf8 (Text.unpack result) of
    Right p -> pure p
    Left e -> fail $ OsPath.showEncodingException e

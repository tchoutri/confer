module Confer.Config.Evaluator ( loadConfiguration ) where

import Control.Monad (void)
import Data.Vector (Vector)
import Effectful
import Data.Vector qualified as Vector
import HsLua.Core (Exception)
import HsLua.Core qualified as Lua
import HsLua.Marshalling qualified as Lua
import HsLua.Marshalling.Peek
import HsLua.Aeson qualified as Lua

import Confer.Config.Types

loadConfiguration :: (IOE :> es) => Eff es (Result (Vector Deployment))
loadConfiguration= liftIO $ Lua.run $ do
  Lua.openlibs -- load the default Lua packages
  void $ Lua.dofile (Just "./doc/confer_example.lua")  -- load and run the program
  Lua.runPeeker peekConfig Lua.top

peekConfig :: Peeker Exception (Vector Deployment)
peekConfig index = Lua.retrieving "config" $ do
  Vector.fromList <$> Lua.peekViaJSON index

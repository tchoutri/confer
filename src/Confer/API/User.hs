module Confer.API.User where

import Effectful
import Effectful.FileSystem (FileSystem)
import Effectful.FileSystem qualified as FileSystem
import HsLua.Core (Exception)
import HsLua.Core qualified as Lua
import HsLua.Marshalling
import HsLua.Packaging

mkUserModule :: FileSystem :> es => Eff es (Module Exception)
mkUserModule = do
  home <- mkHome
  pure
    Module
      { moduleName = "user"
      , moduleFields =
          [ home
          ]
      , moduleFunctions = []
      , moduleOperations = []
      , moduleTypeInitializers = []
      , moduleDescription =
          "Access to the user's information"
      }

-- | Module field containing the machine's hostname.
mkHome :: FileSystem :> es => Eff es (Field Exception)
mkHome = do
  homeDirectory <- FileSystem.getHomeDirectory
  pure
    Field
      { fieldName = "home"
      , fieldType = "string"
      , fieldDescription =
          "The user's HOME directory"
      , fieldPushValue = pushString homeDirectory
      }

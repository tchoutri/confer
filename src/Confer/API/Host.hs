module Confer.API.Host where

import Effectful
import HsLua.Core (Exception)
import HsLua.Core qualified as Lua
import HsLua.Marshalling
import HsLua.Module.System (arch, os)
import HsLua.Packaging
import Network.HostName

mkHostModule :: (IOE :> es) => Eff es (Module Exception)
mkHostModule = do
  hostname <- mkHostname
  pure Module
    { moduleName = "host"
    , moduleFields = 
          [ arch
          , os
          , hostname
          ]
    , moduleFunctions = []
    , moduleOperations = []
    , moduleTypeInitializers = []
    , moduleDescription =
        "Access to the system's information and file functionality."
    }

-- | Module field containing the machine's hostname.
mkHostname :: (IOE :> es) => Eff es (Field Exception)
mkHostname = do
  hostnameString <- liftIO getHostName
  pure Field
    { fieldName = "hostname"
    , fieldType = "string"
    , fieldDescription =
        "The machine's hostname."
    , fieldPushValue = pushString hostnameString
    }

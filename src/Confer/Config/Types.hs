module Confer.Config.Types
  ( Fact (..)
  , Deployment (..)
  , DeploymentOS (..)
  , maybeToDeploymentOS
  , DeploymentArchitecture (..)
  , maybeToDeploymentArchitecture
  ) where

import Data.Maybe
import Data.Text (Text)
import Data.Text.Display
import Data.Text.Internal.Builder qualified as Builder
import Data.Vector (Vector)
import System.OsPath (OsPath)
import System.OsPath qualified as OsPath

import GHC.Generics

data Fact = Fact
  { name :: Text
  , source :: OsPath
  , destination :: OsPath
  }
  deriving stock (Show, Eq)

instance Display Fact where
  displayBuilder Fact{name, source, destination} =
    Builder.fromText name
      <> ": "
      <> Builder.fromString (fromJust (OsPath.decodeUtf source))
      <> " ~> "
      <> Builder.fromString (fromJust (OsPath.decodeUtf destination))

data Deployment = Deployment
  { hostname :: Maybe Text
  , architecture :: DeploymentArchitecture
  , os :: DeploymentOS
  , facts :: Vector Fact
  }
  deriving stock (Show, Eq)

data DeploymentOS
  = AllOS
  | OS Text
  deriving stock (Show, Eq)

maybeToDeploymentOS :: Maybe Text -> DeploymentOS
maybeToDeploymentOS Nothing = AllOS
maybeToDeploymentOS (Just t) = OS t

instance Display DeploymentOS where
  displayBuilder (OS t) = displayBuilder t
  displayBuilder AllOS = "all systems"

data DeploymentArchitecture
  = AllArchs
  | Arch Text
  deriving stock (Show, Eq)

instance Display DeploymentArchitecture where
  displayBuilder (Arch t) = displayBuilder t
  displayBuilder AllArchs = "all architectures"

maybeToDeploymentArchitecture :: Maybe Text -> DeploymentArchitecture
maybeToDeploymentArchitecture Nothing = AllArchs
maybeToDeploymentArchitecture (Just t) = Arch t

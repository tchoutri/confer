module Confer.Config.Types where

import Data.Text (Text)
import Data.Text.Display
import Data.Vector (Vector)
import System.OsPath (OsPath, (</>))
import Data.Maybe
import Data.Text.Internal.Builder qualified as Builder
import System.OsPath qualified as OsPath

import GHC.Generics

data Fact = Fact 
  { name :: Text
  , source :: OsPath
  , destination :: OsPath
  }
  deriving stock (Show, Eq, Generic)

instance Display Fact where
  displayBuilder Fact{name, source, destination} = 
    "[+] " 
      <> (Builder.fromText name)
      <> ": Linking to "
      <> Builder.fromString (fromJust (OsPath.decodeUtf (destination </> source)))

data Deployment = Deployment 
  { hostname :: Maybe Text
  , architecture :: Maybe Text
  , os :: Maybe Text
  , facts :: Vector Fact
  }
  deriving stock (Show, Eq, Generic)

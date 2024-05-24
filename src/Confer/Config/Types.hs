{-# OPTIONS_GHC -Wno-orphans #-}
module Confer.Config.Types where

import Data.Text (Text)
import Data.Text qualified as Text
import System.IO (utf8)
import Data.Vector (Vector)
import System.OsPath (OsPath)
import System.OsPath.Encoding qualified as OsPath
import System.OsPath qualified as OsPath
import Data.Aeson
import GHC.Generics

data Fact = Fact 
  { name :: Text
  , source :: OsPath
  , destination :: OsPath
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON)

data Deployment = Deployment 
  { hostname :: Maybe Text
  , architecture :: Maybe Text
  , os :: Maybe Text
  , facts :: Vector Fact
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON)

instance FromJSON OsPath where
  parseJSON = withText "filepath" $ \t -> 
    case OsPath.encodeWith utf8 utf8 (Text.unpack t) of
      Right p -> pure p
      Left e -> fail $ OsPath.showEncodingException e
    

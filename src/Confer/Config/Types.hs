{-# OPTIONS_GHC -Wno-orphans #-}
module Confer.Config.Types where

import Data.Text (Text)
import Data.Vector (Vector)
import System.OsPath (OsPath)
import GHC.Generics

data Fact = Fact 
  { name :: Text
  , source :: OsPath
  , destination :: OsPath
  }
  deriving stock (Show, Eq, Generic)

data Deployment = Deployment 
  { hostname :: Maybe Text
  , architecture :: Maybe Text
  , os :: Maybe Text
  , facts :: Vector Fact
  }
  deriving stock (Show, Eq, Generic)

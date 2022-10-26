{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Config (
  readConfig
  , Config(..)
  , DBConfig(..)
  , APIConfig(..)
  , ImaggaConfig(..)
  ) where

import Dhall

data ImaggaConfig = ImaggaConfig
  { apiKey :: Text
  , apiSecret :: Text
  }
  deriving (Show, Generic)

newtype APIConfig = APIConfig
  { port :: Int
  }
  deriving (Show, Generic)

data DBConfig = DBConfig
  { dbName :: Text
  , dbHost :: Text
  , dbUser :: Text
  , dbPass :: Text
  }
  deriving (Show, Generic)

data Config = Config
  { api :: APIConfig
  , database :: DBConfig
  , imagga :: ImaggaConfig
  }
  deriving (Show, Generic)

instance FromDhall ImaggaConfig
instance FromDhall APIConfig
instance FromDhall DBConfig
instance FromDhall Config

readConfig :: IO Config
readConfig = input auto "./config.dhall"

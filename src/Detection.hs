{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Detection (tagImage) where

import Config

import Control.Lens
import Data.Aeson
import Data.Aeson.Types
import Data.Text.Encoding (encodeUtf8)
import Data.Text (pack)
import Network.Wreq

newtype TagResult = TagResult
  { getTagResult :: String
  } deriving (Show)

newtype TagResults = TagResults
  { getTagResults :: [TagResult]
  } deriving (Show)

instance FromJSON TagResult where
  parseJSON (Object v) = TagResult <$> (v .: "tag" >>= (.: "en"))
  parseJSON v = typeMismatch "TagResult" v

instance FromJSON TagResults where
  parseJSON (Object v) = TagResults <$> (v .: "result" >>= (.: "tags"))
  parseJSON v = typeMismatch "TagResults" v

tagImage :: ImaggaConfig -> String ->  IO [String]
tagImage ImaggaConfig { apiKey, apiSecret } imageUrl = do
  let user = encodeUtf8 apiKey
      pass = encodeUtf8 apiSecret
      opts = defaults & auth ?~ basicAuth user pass
                      & param "image_url" .~ [pack imageUrl]
  r <- asJSON =<< getWith opts "https://api.imagga.com/v2/tags"
  return . map getTagResult . getTagResults $ r ^. responseBody

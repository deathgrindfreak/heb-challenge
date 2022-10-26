{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (when, unless)
import Control.Monad.IO.Class
import Data.Maybe (fromJust, isJust, isNothing, fromMaybe)
import Data.Pool (Pool)
import Data.List.Split (splitOn)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Network.HTTP.Types.Status (badRequest400, notFound404)
import Network.URI
import Web.Scotty

import qualified Config as C
import Detection
import Db
import Model

routes :: Pool Connection -> C.ImaggaConfig -> ScottyM ()
routes pool imaggaConfig = do
  get "/images" (getImagesHandler pool)
  get "/images/:id" (getImageHandler pool)
  post "/images" (insertImageHandler pool imaggaConfig)

getImagesHandler :: Pool Connection -> ActionM ()
getImagesHandler pool = do
  ts <- fmap (splitOn ",") . findParam "objects" <$> params
  images <- liftIO $ case ts of
    Just searchTags -> searchImages pool searchTags
    Nothing -> fetchImages pool
  json images

getImageHandler :: Pool Connection -> ActionM ()
getImageHandler pool = do
  maybeImageId <- U.fromText <$> (param "id" :: ActionM T.Text)

  when (isNothing maybeImageId) $
    raiseStatus badRequest400 "Improper UUID"

  maybeImage <- liftIO $ fetchImage pool (fromJust maybeImageId)

  when (isNothing maybeImage) $
    raiseStatus notFound404 "Image Not Found"

  json $ fromJust maybeImage

insertImageHandler :: Pool Connection -> C.ImaggaConfig -> ActionM ()
insertImageHandler pool c = do
  (imageUrl, formLabel, detectionOn) <- validateFormData <$> params

  unless (maybe False isURI imageUrl) $
    raiseStatus badRequest400 "Image URL is required"

  let url = fromJust imageUrl

  tagValues <- if detectionOn
    then liftIO $ tagImage c url
    else return []

  -- Use the image name as the label if the user didn't pass one in
  let l = fromMaybe (parseFileName url) formLabel

  image <- liftIO . insertImage pool . NewImage l $ map Tag tagValues
  json image
  where
    validateFormData :: [Param] -> (Maybe String, Maybe String, Bool)
    validateFormData ps =
      ( findParam "url" ps
      , findParam "label" ps
      , isJust . findParam "discover" $ ps
      )

    parseFileName = last . splitOn "/" . uriPath . fromJust . parseURI

findParam :: LT.Text -> [Param] -> Maybe String
findParam key ps = case take 1 . filter ((== key) . fst) $ ps of
  [] -> Nothing
  ((_, value) : _) -> Just (LT.unpack value)

startServer :: C.Config -> IO ()
startServer config = do
  pool <- newConnectionPool (C.database config)
  scotty (C.port . C.api $ config) (routes pool (C.imagga config))

main :: IO ()
main = C.readConfig >>= startServer

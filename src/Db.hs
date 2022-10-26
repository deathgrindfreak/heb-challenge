{-# LANGUAGE OverloadedStrings #-}

module Db ( newConnectionPool
          , fetchImage
          , fetchImages
          , searchImages
          , insertImage
          ) where

import qualified Config as C
import qualified Model as M

import Data.Int
import Database.PostgreSQL.Simple
import Data.Pool(Pool, createPool, withResource)
import Data.Text (unpack)
import Data.UUID

newConnectionPool :: C.DBConfig -> IO (Pool Connection)
newConnectionPool config =
  let conn =
        connect
          defaultConnectInfo
            { connectHost = unpack . C.dbHost $ config,
              connectUser = unpack . C.dbUser $ config,
              connectPassword = unpack . C.dbPass $ config,
              connectDatabase = unpack . C.dbName $ config
            }
   in createPool conn close 1 40 10

fetchImages :: Pool Connection -> IO [M.Image]
fetchImages pool = withResource pool $ \conn ->
  query_
    conn
    "SELECT i.id, i.label, array_remove(array_agg(t.name), NULL) AS name \
    \ FROM image i\
    \ LEFT JOIN image_tag it ON i.id = it.image_id\
    \ LEFT JOIN tag t ON t.id = it.tag_id\
    \ GROUP BY i.id"

fetchImage :: Pool Connection -> UUID -> IO (Maybe M.Image)
fetchImage pool imageId = withResource pool $ \conn -> do
  r <-
    query
      conn
      "SELECT i.id, i.label, array_remove(array_agg(t.name), NULL) AS name \
      \ FROM image i\
      \ LEFT JOIN image_tag it ON i.id = it.image_id\
      \ LEFT JOIN tag t ON t.id = it.tag_id\
      \ AND i.id = ?\
      \ GROUP BY i.id"
      $ Only imageId
  return . one $ r
  where one (x : _) = Just x
        one _ = Nothing

searchImages :: Pool Connection -> [String] -> IO [M.Image]
searchImages pool tags = withResource pool $ \conn ->
    query
      conn
      "SELECT i.id, i.label, array_agg(t.name)\
      \ FROM image i, image_tag it, tag t\
      \ WHERE i.id = it.image_id\
      \ AND it.tag_id = t.id\
      \ AND t.name IN ?\
      \ GROUP BY i.id\
      \ HAVING COUNT(t.id) = ?"
      (In tags, length tags)

insertImage :: Pool Connection -> M.NewImage -> IO M.Image
insertImage pool (M.NewImage label tags) = withResource pool insertImageAndTags
  where
    insertImageAndTags :: Connection -> IO M.Image
    insertImageAndTags conn = withTransaction conn $ do
      imageId <- insertNewImage conn label
      tagIds <- insertTags conn tags
      _ <- insertTagLinks conn imageId tagIds
      return $ M.Image imageId label tags

    insertNewImage :: Connection -> String -> IO UUID
    insertNewImage c l = do
      M.ImageId imageId <-
        head
          <$> query c "INSERT INTO image (label) VALUES (?) RETURNING id" (Only l)
      return imageId

    insertTags :: Connection -> [M.Tag] -> IO [M.TagId]
    insertTags c ts = do
      let tagSql =
            "INSERT INTO tag (name)\
            \ VALUES (?)\
            \ ON CONFLICT (name) DO\
            \ UPDATE SET name = EXCLUDED.name\
            \ RETURNING id"

      returning c tagSql ts

    insertTagLinks :: Connection -> UUID -> [M.TagId] -> IO Int64
    insertTagLinks c imageId tagIds =
      executeMany c "INSERT INTO image_tag VALUES (?, ?)" $ zip (repeat imageId) tagIds

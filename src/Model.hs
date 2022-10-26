{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Model
  ( Image (..),
    Tag (..),
    NewImage (..),
    ImageId (..),
    TagId (..)
  )
where

import Data.UUID
import Data.Aeson
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.Types
import GHC.Generics (Generic)
import Data.Maybe (fromJust)

newtype Tag = Tag { name :: String} deriving (Show, Generic, FromRow, ToRow)

data Image = Image
  { id :: UUID
  , label :: String
  , tags :: [Tag]
  }
  deriving (Show, Generic)

instance FromRow Image where
  fromRow = Image <$> field <*> field <*> (map Tag . fromPGArray <$> field)

newtype ImageId = ImageId UUID deriving (Show, Generic, FromRow)
newtype TagId = TagId Integer deriving (Show, Generic, FromRow, ToRow)

instance ToField TagId where
  toField (TagId tagId) = toField tagId

data NewImage = NewImage String [Tag] deriving (Show, Generic)

instance ToJSON Tag where
instance ToJSON Image where

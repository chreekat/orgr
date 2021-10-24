{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Orgr.Item where

import Data.Text (Text)
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.ToField
import Database.SQLite.Simple.ToRow

newtype Item = Item {unItem :: Text}
    deriving stock (Show, Eq)
    deriving newtype (FromField, ToField)

-- Lord save me from these names
data ItemDb = ItemDb {itemDbId :: Int, itemDbItem :: Item}
    deriving (Show, Eq)

leItem :: ItemDb -> Text
leItem = unItem . itemDbItem

instance FromRow ItemDb where
    fromRow = ItemDb <$> field <*> field

instance ToRow ItemDb where
    toRow (ItemDb id_ i) = toRow (id_, i)

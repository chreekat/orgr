{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Orgr.Item where

import Data.Text (Text)
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.ToField

newtype Item = Item {unItem :: Text}
    deriving stock (Show, Eq)
    deriving newtype (FromField, ToField)

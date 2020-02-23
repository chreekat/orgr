{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Prelude hiding (putStr, putStrLn, getLine, unlines)

import Control.Exception.Safe
import Control.Monad (forever)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Database.SQLite.Simple
import Database.SQLite.Simple.ToField
import Database.SQLite.Simple.FromField
import GHC.Stack
import Data.Text (Text, unlines)
import Data.Text.IO (putStr, putStrLn, getLine)

newtype Item = Item { unItem :: Text }
    deriving stock Show
    deriving newtype (FromField, ToField)

item1 = Item "Make a basic model for Orgr"

data Views
    = CaptureIn
    | ReviewIn


newtype Orgr a = Orgr { runOrgr :: IO a }
    deriving newtype (Functor, Applicative, Monad, MonadIO)

main :: HasCallStack => IO ()
main = forever $ do
    -- Set up a db
    conn <- open "test.db"
    execute_ conn "create table if not exists inbox (id integer primary key, item text)"
    items <- query_ conn "select item from inbox" :: IO [Only Item]
    putStrLn "######## ITEMS ###########"
    putStrLn (unlines (map (unItem . fromOnly) items))
    putStrLn "##########################"
    item <- fmap Item $ liftIO $ do
        putStr "IN> "
        getLine
    execute conn "insert into inbox (item) values (?)" (Only item)

{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Orgr where

import Prelude hiding (putStr, putStrLn, getLine, unlines)

import Control.Exception.Safe
import Control.Monad (forever)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Lens
import Data.Text (Text, unlines)
import Data.Text.IO (putStr, putStrLn, getLine)
import Database.SQLite.Simple
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.ToField
import GHC.Stack
import Monomer

newtype Item = Item { unItem :: Text }
    deriving stock Show
    deriving newtype (FromField, ToField)

{-

Entity Component System

Components: Things an item (entity) can have or be

 * due date
 * occurrence(s)
 * email identifier
 * box-of-stuff-style links to other items
 * relative importance
 * location on a list
 * summary
 * long description
 * content
 * file attachments
 * title
 * snooze timer (e.g. to implement tickler)
 * owner/author
 * recipient and other email headers
 * spaced repitition attributes

Types of items (implemented structurally by having or not having the above components):

 * Project
 * Task
 * Goal
 * Meeting
 * Invitation
 * Topic for thinking/writing about
 * GTD "task context"
 * Email

Structure for items

 * Archive
 * Reference
 * To-do lists
 * Thinking/Reading lists

-}

data Views
    = CaptureIn
    | ReviewIn


newtype Orgr a = Orgr { runOrgr :: IO a }
    deriving newtype (Functor, Applicative, Monad, MonadIO)

main1 :: IO ()
main1 = forever $ do
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


{-

Actions - how to handle Inbox items.

Step 0: make processing notes - a scratchpad for brainstorming before taking
action.

Step 1: take action:

- Create a goal (schedule - dod - importance)
- Create a task (dod - importance - urgency)
- Archive/reference it
- Schedule a calendar event
- Schedule a reminder aka snooze it aka tickle it


-}

data ShowInboxModel = ShowInboxModel
    deriving (Eq, Show)

makeLenses 'ShowInboxModel

data AppEvent = AppEvent

type TopApp a = a ShowInboxModel AppEvent

handler
    :: TopApp WidgetEnv
    -> TopApp WidgetNode
    -> ShowInboxModel
    -> AppEvent
    -- For some reason, [TopApp AppEventResponse] results in "The type synonym
    -- AppEventResponse should have 2 arguments, but has been given none".
    -> [AppEventResponse ShowInboxModel AppEvent]
handler _ _ _ _ = []
buildUI _ _ = box_ [alignCenter, alignMiddle] (vstack [label "Hello world"])
main = showInbox
showInbox = do
    -- Set up a db
    conn <- open "test.db"
    execute_ conn "create table if not exists inbox (id integer primary key, item text)"
    items <- query_ conn "select item from inbox" :: IO [Only Item]
    startApp ShowInboxModel handler buildUI config
    where
    config =
        -- FIXME: use Cabal paths
        [ appFontDef "Regular" "./assets/fonts/Roboto-Regular.ttf"
        , appWindowTitle "Hello world"
        , appTheme darkTheme
        ]

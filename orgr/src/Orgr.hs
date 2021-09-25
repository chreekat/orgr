{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Orgr where

import Prelude hiding (getLine, putStr, putStrLn, unlines)

import Control.Exception.Safe
import Control.Monad (forever)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text, unlines)
import Data.Text.IO (getLine, putStr, putStrLn)
import Database.SQLite.Simple
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.ToField
import GHC.Stack
import Monomer

newtype Item = Item {unItem :: Text}
    deriving stock (Show, Eq)
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
 * next action

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

-- * Details about types of items.

Goal.
----

A goal has a description of "done".
It can be on a task list, in which case it is also an Action Item, and SHOULD have a due date and priority.
It can be on the projects list, in which case it also a Project (duh), and SHOULD have a due date and (next action or waiting-for)
It can be tickled.
It can not be on those lists, in which case it is automatically on the "Someday/Maybe" list.

In terms of converting Inbox Items into Goals, one needs to know:

* Goal description incl. definition of done
* Is there a due date
* Is there a priority
* Should it be tickled
* Is it a project
    * What is the next-action (TBD, need to define next-actions) or waiting-for?
* Does it belong on a task list?

Sometimes an Inbox Item is inspiration for a goal, but isn't the goal itself. In
that case, the original idea can be discarded or attached as a note.

-}

data Views
    = CaptureIn
    | ReviewIn

newtype Orgr a = Orgr {runOrgr :: IO a}
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
    item <- fmap Item $
        liftIO $ do
            putStr "IN> "
            getLine
    execute conn "insert into inbox (item) values (?)" (Only item)

{-

Actions - how to handle Inbox items.

Action 0: Make processing notes - a scratchpad for brainstorming before taking
action.

Action 1: Edit it

Action 2: Take action:

- Create a goal (schedule - dod - importance)
- Create a task (dod - importance - urgency)
- Archive/reference it
- Schedule a calendar event
- Schedule a reminder aka snooze it aka tickle it

-}

data EditingProcessedItem = Editing | NotEditing
    deriving (Eq, Show)

data ProcessInboxModel = ProcessInboxModel [Item] EditingProcessedItem
    deriving (Eq, Show)

data AppEvent = Nop | UpdateItem Text

handler ::
    TopApp WidgetEnv ->
    TopApp WidgetNode ->
    ProcessInboxModel ->
    AppEvent ->
    -- For some reason, [TopApp AppEventResponse] results in "The type synonym
    -- AppEventResponse should have 2 arguments, but has been given none".
    [AppEventResponse ProcessInboxModel AppEvent]
handler wenv node model = \case
    Nop -> []
    UpdateItem t -> updateItem t model

updateItem t (ProcessInboxModel is s) =
    pure $ Model $ ProcessInboxModel (Item t : tail is) s

buildProcessInboxUI _ model@(ProcessInboxModel is editing) =
    let thing = case editing of
            NotEditing -> label (unItem (head is))
            Editing -> textFieldV (unItem (head is)) UpdateItem
     in box_ [alignCenter, alignMiddle] thing

type TopApp a = a ProcessInboxModel AppEvent

main = do
    -- Set up a db
    conn <- open "test.db"
    execute_ conn "create table if not exists inbox (id integer primary key, item text)"
    items <- fmap fromOnly <$> query_ conn "select item from inbox"
    startApp (initial items) handler buildProcessInboxUI config
  where
    initial is = ProcessInboxModel is Editing
    config =
        -- FIXME: use Cabal paths
        [ appFontDef "Regular" "./assets/fonts/Roboto-Regular.ttf"
        , appWindowTitle "Orgr"
        , appTheme darkTheme
        ]

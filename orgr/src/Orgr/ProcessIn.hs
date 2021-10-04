{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Orgr.ProcessIn where

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

import Orgr.Item

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

-- Model

data EditingProcessedItem = Editing | NotEditing
    deriving (Eq, Show)

data ProcessInboxModel = ProcessInboxModel [Item] EditingProcessedItem
    deriving (Eq, Show)

-- View

buildProcessInboxUI _ model@(ProcessInboxModel is editing) =
    let thing = case editing of
            NotEditing -> label (unItem (head is))
            Editing -> textFieldV (unItem (head is)) UpdateItem
     in box_ [alignCenter, alignMiddle] thing

-- Whatever

data AppEvent = Nop | UpdateItem Text

type TopApp a = a ProcessInboxModel AppEvent

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

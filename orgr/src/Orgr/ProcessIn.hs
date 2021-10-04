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
import Monomer hiding (Model)
import qualified Monomer

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

data Model = Model [Item] EditingProcessedItem
    deriving (Eq, Show)

-- View

buildUI _ model@(Model is editing) =
    let thing = case editing of
            NotEditing -> label (unItem (head is))
            Editing -> textFieldV (unItem (head is)) UpdateItem
     in box_ [alignCenter, alignMiddle] thing

-- Whatever

data Event = Nop | UpdateItem Text

type TopApp a = a Model Event

handler ::
    TopApp WidgetEnv ->
    TopApp WidgetNode ->
    Model ->
    Event ->
    -- For some reason, [TopApp AppEventResponse] results in "The type synonym
    -- AppEventResponse should have 2 arguments, but has been given none".
    [AppEventResponse Model Event]
handler wenv node model = \case
    Nop -> []
    UpdateItem t -> updateItem t model

updateItem t (Model is s) =
    pure $ Monomer.Model $ Model (Item t : tail is) s

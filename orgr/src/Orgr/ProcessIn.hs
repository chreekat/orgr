{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Orgr.ProcessIn where

import Prelude hiding (getLine, putStr, putStrLn, unlines)

import Data.Text (Text)
import Database.SQLite.Simple
import Debug.Trace
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

data Model = Model {modelItems :: [ItemDb], modelEditMode :: EditingProcessedItem}
    deriving (Eq, Show)

data UserAction = Edit | Save

-- TODO: Make KeyMap more sophisticated. Modes ala vim, submaps ala emacs. A lot
-- could be generated from the list of user actions alone if it was more
-- sophisticated.
type KeyMap = [(Text, UserAction)]

viewKeymap, editKeymap :: KeyMap
viewKeymap =
    [("e", Edit)]
editKeymap =
    [("Enter", Save)]

data Event = Nop | UpdateItem Text | HandleUser UserAction

-- View

buildUI :: WidgetEnv Model Event -> Model -> WidgetNode Model Event
buildUI _ _model@(Model is editing) =
    let (thing, kmap) = case editing of
            NotEditing ->
                ( label (leItem (head is))
                , viewKeymap
                )
            Editing ->
                ( textFieldV (leItem (head is)) UpdateItem `nodeKey` "edit-box"
                , editKeymap
                )
     in keystroke_
            (fmap (fmap HandleUser) kmap)
            -- FIXME: This doesn't do what I expect.
            [ignoreChildrenEvts]
            $ box_ [alignCenter, alignMiddle] thing

-- Whatever

type TopApp a = a Model Event

handler ::
    TopApp WidgetEnv ->
    TopApp WidgetNode ->
    Model ->
    Event ->
    -- For some reason, [TopApp AppEventResponse] results in "The type synonym
    -- AppEventResponse should have 2 arguments, but has been given none".
    [AppEventResponse Model Event]
handler wenv _node model = \case
    Nop -> []
    UpdateItem t -> updateItem t model
    HandleUser x
        | Save <- x ->
            trace
                "Save"
                [ Monomer.Model model{modelEditMode = NotEditing}
                , -- TODO?: This should be a Report, not a Task. Centralize persistence.
                  let (ItemDb id_ i) = head (modelItems model)
                   in Monomer.Task $
                        Nop <$ do
                            traceIO "Writing db"
                            conn <- open "test.db"
                            -- TODO: centralize SQL statements
                            execute conn "update inbox set item = ? where id = ?" (i, id_)
                            close conn
                ]
        | Edit <- x ->
            trace
                "Edit"
                [ Monomer.Model model{modelEditMode = Editing}
                , -- FIXME: This doesn't do what I expect.
                  setFocusOnKey wenv "edit-box"
                ]

updateItem :: Applicative f => Text -> Model -> f (AppEventResponse Model Event)
updateItem t (Model is s) =
    let ItemDb id_ _ = head is
     in pure $ Monomer.Model $ Model (ItemDb id_ (Item t) : tail is) s

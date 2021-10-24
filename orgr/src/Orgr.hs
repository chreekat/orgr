{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Orgr where

import Prelude hiding (getLine, putStr, putStrLn, unlines)

import Data.Text (Text)
import Database.SQLite.Simple
import Monomer

import qualified Orgr.ProcessIn as ProcessIn

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

main :: IO ()
main = do
    -- Set up a db
    conn <- open "test.db"
    execute_ conn "create table if not exists inbox (id integer primary key, item text)"
    items <- query_ conn "select id, item from inbox"
    close conn
    startApp (initial items) ProcessIn.handler ProcessIn.buildUI config
  where
    initial is = ProcessIn.Model is ProcessIn.NotEditing
    config =
        -- FIXME: use Cabal paths
        [ appFontDef "Regular" "./assets/fonts/Roboto-Regular.ttf"
        , appWindowTitle "Orgr"
        , appTheme darkTheme
        ]

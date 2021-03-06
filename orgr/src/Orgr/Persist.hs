-- Module about persisting Orgr data.
module Orgr.Persist (module Orgr.Persist) where

-- Trying to decide how to write my first entity-component-system system.
-- In Orgr, we have items as the basic thing. Maybe now's the time to change
-- the word to entity. But entity feels too alive. Orgr deals with items, not
-- entities. The UI will talk about items, so I'll start there. I can rename
-- globally if I ever want to.

-- item
-- ----
-- id    int
--
--
--
-- item_component
-- --------------
-- id             int   -- maybe?
-- item_id        int not null
-- component_type int not null
-- component_id   int not null
--
--
-- cmp_description
-- ---------------
-- id           int
-- description  string
--
--
-- cmp_title
-- ---------
-- id           int
-- title        string
--
--
--
-- and then
--
-- getItem =
--
-- select i.id, desc.description, tit.title
-- from item i
-- left join item_component ic_desc
-- on ic_desc.item_id = i.id
-- left join item_component ic_tit
-- on ic_tit.item_id = i.id
-- join cmp_description desc
-- on desc.id = ic_desc.component_id
-- join cmp_title tit
-- on tit.id = ic_tit.component_id
--

-- Module about persisting Orgr data.
-- module Orgr.Persist
--     (module Orgr.Persist) where
-- 
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
CREATE TABLE item (item_id integer primary key);
CREATE TABLE cmp_description(
    desc_id integer primary key,
    item_id integer references item(id),
    description string);
CREATE TABLE cmp_title(
    title_id integer integer primary key,
    item_id integer references item(id),
    title string);
CREATE TABLE cmp_duedate(
    duedate_id integer integer primary key,
    item_id integer references item(id),
    duedate string);

insert into item default values;
insert into cmp_title (item_id, title) values (1, "I am a brief item");

insert into item default values;
insert into cmp_title (item_id, title) values (2, "I am a longer item");
insert into cmp_description (item_id, description)
values (2, "This is my long ass description");

insert into item default values;
insert into cmp_description (item_id, description)
values (3, "I am a headless item");
insert into cmp_duedate (item_id, duedate)
values (3, "2021-12-03 10:00:00");

insert into item default values;

create view basic_item as
select item_id, title, description
from item i
left join cmp_title using (item_id)
left join cmp_description using (item_id)
where
title is not null or
description is not null
;

select * from basic_item;

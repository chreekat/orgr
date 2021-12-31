-- Module about persisting Orgr data.
-- module Orgr.Persist
--     (module Orgr.Persist) where
-- 

CREATE TABLE item (iid integer primary key);

CREATE TABLE cmp (
    iid references item(iid),
    cmp string,
    val);

-- description, title, duedate, tag

insert into item default values;
insert into cmp (iid, cmp, val) values (1, "title", "I am a brief item");

insert into item default values;
insert into cmp (iid, cmp, val) values
    (2, "title", "I am a longer item"),
    (2, "description", "This is my long ass description");

insert into item default values;
insert into cmp (iid, cmp, val) values
    (3, "description", "I am a headless item"),
    (3, "duedate", "2021-12-03 10:00:00");

-- 4
insert into item default values;

insert into item default values;
insert into cmp (iid, cmp, val) values
    (5, "title", "I am todo item"),
    (5, "description", "I am a thing that needs doing."),
    (5, "tag", "TODO"),
    (5, "tag", "At Home");

-- select * from item left join cmp using (iid);

create view item_test_view as
select iid, title, description, duedate, tags
from item
left join (
    select iid, val as description
    from cmp
    where cmp.cmp = 'description'
) c1 using (iid)
left join (
    select iid, val as duedate
    from cmp
    where cmp.cmp = 'duedate'
) c2 using (iid)
left join (
    select iid, val as title
    from cmp
    where cmp.cmp = 'title'
) c3 using (iid)
left join (
    select iid, group_concat(val) as tags
    from cmp
    where cmp.cmp = 'tag'
    group by iid
) c4 using (iid)
;


select * from item_test_view;
select '';
select * from cmp;

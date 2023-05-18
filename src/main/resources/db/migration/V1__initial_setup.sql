create table filterset(
    id integer primary key,
    code text,
    name text
);

create table filterset_library(
    id integer primary key,
    filterset_id integer,
    name text,

    foreign key(filterset_id) references filterset(id)
);

create table index_entry(
    id integer primary key,
    group_id integer,
    lib text,
    name text,
    signature text,
    description text,

    foreign key(group_id) references index_entry(id)
);

create table index_entry_tag(
    id integer primary key,
    index_entry_id integer,
    name text,

    foreign key(index_entry_id) references index_entry(id)
);

create table index_entry_subsignature(
    id integer primary key,
    index_entry_id integer,
    name text,
    signature text,

    foreign key(index_entry_id) references index_entry(id)
);

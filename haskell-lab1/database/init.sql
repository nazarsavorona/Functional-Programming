create table address
(
    id     integer generated always as identity
        constraint address_id
            primary key,
    street varchar(45),
    city   varchar(45)
);

alter table address
    owner to postgres;

create table person
(
    id         integer generated always as identity
        constraint person_id
            primary key,
    name       varchar(45),
    phone      varchar(30),
    address_id integer
        unique
        constraint address_id
            references address
            on update cascade on delete cascade
);

alter table person
    owner to postgres;

create table position
(
    id           integer generated always as identity
        constraint position_id
            primary key,
    name         varchar(45),
    access_level integer
);

alter table position
    owner to postgres;

create table staff
(
    id          integer generated always as identity
        constraint staff_id
            primary key,
    salary      integer,
    person_id   integer
        unique
        constraint person_id
            references person
            on update cascade on delete cascade,
    position_id integer
        constraint position_id
            references position
            on update cascade on delete cascade
);

alter table staff
    owner to postgres;

create table newspaper
(
    id       integer generated always as identity
        constraint newspaper_id
            primary key,
    about    text,
    staff_id integer
        unique
        constraint staff_id
            references staff
            on update cascade on delete cascade
);

alter table newspaper
    owner to postgres;
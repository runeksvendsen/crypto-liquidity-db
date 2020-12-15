CREATE TABLE currencys (
    id serial NOT NULL,
    symbol varchar(16) NOT NULL,

    PRIMARY KEY id,
    CONSTRAINT unique_symbol UNIQUE symbol -- catch bug when looking up existing, and inserting new, currencys
);


create table seating_areas(
    id SERIAL primary key,
    seat_count SMALLINT not null,
    -- (seconds)
    max_duration INTEGER not null,
    -- (seconds) how much time is it okay to delay a booking on the waiting list in order to optimize seat usage.
    -- if the calculated waiting time is e.g. 30 minutes and this constant is e.g. 5 minutes then we report a
    --  waiting time of "30-35 minutes".
    wait_list_delay INTEGER not null,
    max_pax SMALLINT not null,
    -- does the waiting list need to be recalculated?
    dirty_wait_list BOOLEAN not null
);

-- given a pax, what is the expected duration of the seating?
create table expected_durations(
    seating_area__id SERIAL not null,
    pax SMALLINT not null,
    -- (seconds)
    duration INTEGER not null,
    foreign key(seating_area__id)
        references seating_areas(id),
    constraint pk_expected_duration PRIMARY KEY (seating_area__id, pax)
);

create table bookings(
    id SERIAL primary key,
    seating_area__id SERIAL not null,
    -- if false the booking has been deleted
    is_active BOOLEAN not null,
    creation_time TIMESTAMP WITH TIME ZONE not null,
    -- NB: will become GroupID
    guest VARCHAR not null,
    pax SMALLINT not null,
    foreign key(seating_area__id)
        references seating_areas(id)
);

-- a row in this table for a booking and booking.is_active=TRUE
--  then the party is sitting at their table.
create table seated_bookings(
    booking__id SERIAL primary key,
    start_seat SMALLINT not null,
    checkin_time TIMESTAMP WITH TIME ZONE not null,
    expected_duration INTEGER not null,
    foreign key(booking__id)
        references bookings(id)
);

-- a row in this table for a booking, and no row in seated_bookings,
--  and booking.is_active=TRUE then the party can take their seats
create table ready_bookings(
    booking__id SERIAL primary key,
    start_seat SMALLINT not null,
    ready_time TIMESTAMP WITH TIME ZONE not null,
    expected_duration INTEGER not null,
    is_notified BOOLEAN not null,
    foreign key(booking__id)
        references bookings(id)
);

-- each time the waiting list is recalculated
--  a new row is inserted into this table.
-- for a given booking, the row with the most
--  recent 'creation_time' is in force, while
--  previous rows are historic.
create table waiting_bookings(
    id SERIAL primary key,
    booking__id SERIAL not null,
    -- NB: tentative (likely to change when waiting list is recalculated)
    start_seat SMALLINT not null,
    creation_time TIMESTAMP WITH TIME ZONE not null,
    -- (seconds) waiting time was this at 'creation_time'
    waiting_time INTEGER not null,
    expected_duration INTEGER not null,
    foreign key(booking__id)
        references bookings(id)
);

create table reserved_bookings(
    booking__id SERIAL primary key,
    start_seat SMALLINT not null,
    begin_time TIMESTAMP WITH TIME ZONE not null,
    duration INTEGER not null,
    foreign key(booking__id)
        references bookings(id)
);

CREATE UNIQUE INDEX waiting_bookings_booking__id ON waiting_bookings (booking__id, id);
CREATE UNIQUE INDEX bookings_seating_area__id ON bookings (seating_area__id, id);

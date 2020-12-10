CREATE TABLE currencies (
    id serial NOT NULL,
    symbol varchar(16) NOT NULL,

    PRIMARY KEY id,
    CONSTRAINT unique_symbol UNIQUE symbol -- catch bug when looking up existing, and inserting new, currencies
);

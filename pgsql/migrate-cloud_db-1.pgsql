CREATE TABLE migrations_tmp
    ( from_version NUMERIC(5) NOT NULL
    , time TIMESTAMP WITHOUT TIME ZONE NOT NULL DEFAULT NOW()
    );

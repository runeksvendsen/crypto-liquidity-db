CREATE TABLE migrations
    ( from_version NUMERIC(5) NOT NULL
    , time TIMESTAMP WITHOUT TIME ZONE NOT NULL DEFAULT NOW()
    );
ALTER TABLE migrations ADD CONSTRAINT migrations_pkey PRIMARY KEY (from_version);

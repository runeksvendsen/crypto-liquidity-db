CREATE TABLE migrations
    ( from_version SMALLINT NOT NULL
    , time TIMESTAMP WITHOUT TIME ZONE NOT NULL DEFAULT NOW()
    , in_progress BOOLEAN NOT NULL
    );
ALTER TABLE migrations ADD CONSTRAINT migrations_pkey PRIMARY KEY (from_version);

INSERT INTO migrations (from_version, time, in_progress)
    VALUES (-1, DEFAULT, false);

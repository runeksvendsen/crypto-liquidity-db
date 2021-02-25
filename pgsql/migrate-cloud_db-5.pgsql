CREATE TABLE new_paths_tmp
(
    id INT NOT NULL DEFAULT nextval('paths___id___seq'::regclass),
    start__symbol VARCHAR NOT NULL,
    venues VARCHAR[] NOT NULL CHECK (cardinality(venues) = cardinality(currencys)),
    currencys VARCHAR[] NOT NULL CHECK (cardinality(currencys) = cardinality(venues))
);

INSERT INTO new_paths_tmp (id, start__symbol, venues, currencys)
SELECT p.id,
       p.start__symbol,
       array_agg(venue__name ORDER BY pp.index ASC),
       array_agg(currency__symbol ORDER BY pp.index ASC)
FROM paths p JOIN path_parts pp ON pp.path__id = p.id
GROUP BY p.id;

DROP TABLE path_parts;

ALTER TABLE path_qtys DROP CONSTRAINT path_qtys_path__id_fkey;

ALTER TABLE paths DROP CONSTRAINT paths_pkey;
DROP TABLE paths;

ALTER TABLE new_paths_tmp RENAME TO paths;
ALTER TABLE paths ADD CONSTRAINT paths_pkey PRIMARY KEY (id);

ALTER TABLE path_qtys ADD CONSTRAINT path_qtys_path__id_fkey FOREIGN KEY (path__id) REFERENCES paths(id);
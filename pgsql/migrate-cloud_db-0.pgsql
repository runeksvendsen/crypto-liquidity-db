CREATE SEQUENCE calculations___id___seq;

CREATE SEQUENCE paths___id___seq;

CREATE TABLE calculation_parameters (numeraire__symbol VARCHAR NOT NULL, slippage DOUBLE PRECISION NOT NULL);

CREATE TABLE calculations (creation_time TIMESTAMP WITHOUT TIME ZONE NOT NULL, currency__symbol VARCHAR NOT NULL, duration_seconds DOUBLE PRECISION , id INT NOT NULL DEFAULT nextval('calculations___id___seq'::regclass), numeraire__symbol VARCHAR NOT NULL, run__id NUMERIC(10) NOT NULL, slippage DOUBLE PRECISION NOT NULL, start_time TIMESTAMP WITHOUT TIME ZONE );

CREATE TABLE currencys (symbol VARCHAR NOT NULL);

CREATE TABLE path_parts (currency__symbol VARCHAR NOT NULL, index INT NOT NULL, path__id NUMERIC(10) NOT NULL, venue__name VARCHAR NOT NULL);

CREATE TABLE path_qtys (calc__id INT NOT NULL, path__id NUMERIC(10) NOT NULL, price_high DOUBLE PRECISION NOT NULL, price_low DOUBLE PRECISION NOT NULL, qty NUMERIC(20) NOT NULL);

CREATE TABLE paths (id NUMERIC(10) NOT NULL DEFAULT nextval('paths___id___seq'::regclass), part_count NUMERIC(5) NOT NULL, start__symbol VARCHAR NOT NULL);

CREATE TABLE run_currencys (currency__symbol VARCHAR NOT NULL, run__id NUMERIC(10) NOT NULL);

CREATE TABLE venues (name VARCHAR NOT NULL);

ALTER TABLE books ALTER COLUMN id TYPE NUMERIC(10);         -- UNSAFE
ALTER TABLE books ALTER COLUMN run__id TYPE NUMERIC(10);    -- UNSAFE
ALTER TABLE orders ALTER COLUMN book__id TYPE NUMERIC(10);  -- UNSAFE
ALTER TABLE runs ALTER COLUMN id TYPE NUMERIC(10);          -- UNSAFE

ALTER TABLE calculation_parameters ADD CONSTRAINT calculation_parameters_pkey PRIMARY KEY (numeraire__symbol, slippage);

ALTER TABLE calculations ADD CONSTRAINT calculations_pkey PRIMARY KEY (id);

ALTER TABLE currencys ADD CONSTRAINT currencys_pkey PRIMARY KEY (symbol);

ALTER TABLE path_parts ADD CONSTRAINT path_parts_pkey PRIMARY KEY (index, path__id);

ALTER TABLE path_qtys ADD CONSTRAINT path_qtys_pkey PRIMARY KEY (calc__id, path__id);

ALTER TABLE paths ADD CONSTRAINT paths_pkey PRIMARY KEY (id);

ALTER TABLE run_currencys ADD CONSTRAINT run_currencys_pkey PRIMARY KEY (currency__symbol, run__id);

ALTER TABLE venues ADD CONSTRAINT venues_pkey PRIMARY KEY (name);

ALTER TABLE books ADD CONSTRAINT books_run__id_fkey FOREIGN KEY (run__id) REFERENCES runs(id);

ALTER TABLE calculation_parameters ADD CONSTRAINT calculation_parameters_numeraire__symbol_fkey FOREIGN KEY (numeraire__symbol) REFERENCES currencys(symbol);

ALTER TABLE calculations ADD CONSTRAINT calculations_currency__symbol_fkey FOREIGN KEY (currency__symbol) REFERENCES currencys(symbol);

ALTER TABLE calculations ADD CONSTRAINT calculations_numeraire__symbol_fkey FOREIGN KEY (numeraire__symbol) REFERENCES currencys(symbol);

ALTER TABLE calculations ADD CONSTRAINT calculations_run__id_fkey FOREIGN KEY (run__id) REFERENCES runs(id);

ALTER TABLE calculations ADD CONSTRAINT calculations_args_unique UNIQUE (run__id, currency__symbol, numeraire__symbol, slippage);

ALTER TABLE orders ADD CONSTRAINT orders_book__id_fkey FOREIGN KEY (book__id) REFERENCES books(id);

ALTER TABLE path_parts ADD CONSTRAINT path_parts_currency__symbol_fkey FOREIGN KEY (currency__symbol) REFERENCES currencys(symbol);

ALTER TABLE path_parts ADD CONSTRAINT path_parts_path__id_fkey FOREIGN KEY (path__id) REFERENCES paths(id);

ALTER TABLE path_parts ADD CONSTRAINT path_parts_venue__name_fkey FOREIGN KEY (venue__name) REFERENCES venues(name);

ALTER TABLE path_qtys ADD CONSTRAINT path_qtys_calc__id_fkey FOREIGN KEY (calc__id) REFERENCES calculations(id);

ALTER TABLE path_qtys ADD CONSTRAINT path_qtys_path__id_fkey FOREIGN KEY (path__id) REFERENCES paths(id);

ALTER TABLE paths ADD CONSTRAINT paths_start__symbol_fkey FOREIGN KEY (start__symbol) REFERENCES currencys(symbol);

ALTER TABLE run_currencys ADD CONSTRAINT run_currencys_currency__symbol_fkey FOREIGN KEY (currency__symbol) REFERENCES currencys(symbol);

ALTER TABLE run_currencys ADD CONSTRAINT run_currencys_run__id_fkey FOREIGN KEY (run__id) REFERENCES runs(id);

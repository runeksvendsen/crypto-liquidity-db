-- https://stackoverflow.com/a/24195624/700597
CREATE OR REPLACE FUNCTION trg_notify_after()
  RETURNS trigger AS
$func$
BEGIN
   PERFORM pg_notify(TG_TABLE_NAME, TG_OP);
   RETURN NULL;
END
$func$  LANGUAGE plpgsql;

-- runs
DROP TRIGGER IF EXISTS runs_insert
  	ON runs;
CREATE TRIGGER runs_insert
	AFTER INSERT
	ON runs
	FOR EACH STATEMENT
	EXECUTE PROCEDURE trg_notify_after();

-- run_currencys
DROP TRIGGER IF EXISTS run_currencies_insert
  	ON run_currencys;
CREATE TRIGGER run_currencies_insert
	AFTER INSERT
	ON run_currencys
	FOR EACH STATEMENT
	EXECUTE PROCEDURE trg_notify_after();

-- calculations
DROP TRIGGER IF EXISTS calculations_insert
  	ON calculations;
CREATE TRIGGER calculations_insert
	AFTER INSERT
	ON calculations
	FOR EACH STATEMENT
	EXECUTE PROCEDURE trg_notify_after();

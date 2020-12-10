CREATE OR REPLACE FUNCTION channel_notify(channel text)
	RETURNS trigger AS
$$
BEGIN
  PERFORM pg_notify(channel);
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER runs_insert
	AFTER INSERT
	ON runs
	FOR EACH STATEMENT
EXECUTE PROCEDURE pg_notify("runs");

CREATE TRIGGER run_currencies_insert
	AFTER INSERT
	ON run_currencies
	FOR EACH STATEMENT
EXECUTE PROCEDURE pg_notify("run_currencies");

CREATE TRIGGER calculations_insert
	AFTER INSERT
	ON calculations
	FOR EACH STATEMENT
EXECUTE PROCEDURE pg_notify("calculations");

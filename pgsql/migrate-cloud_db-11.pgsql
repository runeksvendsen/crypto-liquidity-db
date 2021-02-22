ALTER TABLE calculations ADD COLUMN total_qty bigint DEFAULT -1;

UPDATE calculations
SET total_qty = subquery.qty_sum
FROM (SELECT calc__id, SUM(qty) as qty_sum FROM path_qtys GROUP BY calc__id) AS subquery
WHERE calculations.id = subquery.calc__id;

ALTER TABLE calculations ALTER COLUMN total_qty DROP DEFAULT;
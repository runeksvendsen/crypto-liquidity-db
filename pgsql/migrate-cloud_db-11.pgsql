CREATE TABLE path_sums (
    calc__id INT NOT NULL,
    buy_qty BIGINT NOT NULL,
    sell_qty BIGINT NOT NULL
);

ALTER TABLE path_sums ADD CONSTRAINT path_sums_pkey PRIMARY KEY (calc__id);
ALTER TABLE path_sums ADD CONSTRAINT path_sums_calculation__id_fkey FOREIGN KEY (calc__id) REFERENCES calculations(id);

INSERT INTO path_sums (calc__id, buy_qty, sell_qty)
SELECT c.id,
       SUM(case when p.start__symbol = c.numeraire__symbol then pq.qty else 0 end),
       SUM(case when p.start__symbol = c.currency__symbol then pq.qty else 0 end)
FROM calculations c
    JOIN path_qtys pq on c.id = pq.calc__id
    JOIN paths p on pq.path__id = p.id
GROUP BY c.id;

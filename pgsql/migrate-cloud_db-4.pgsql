CREATE INDEX finished_calculations ON calculations (numeraire__symbol, slippage, currency__symbol, run__id) WHERE duration_seconds IS NOT NULL;

module Query.Runs
( Query.newestRun
, Run.Run
, Pg.Pg
)
where

import qualified CryptoDepth.OrderBook.Db.Query as Query
import qualified CryptoDepth.OrderBook.Db.Schema.Run as Run
import qualified Database.Beam.Postgres as Pg

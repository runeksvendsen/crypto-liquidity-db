module Query where

import Database
import qualified CryptoDepth.OrderBook.Db.Schema.Run as Run
import qualified Schema.PathQty as PathQty

import Database.Beam
import Database.Beam.Backend (SqlSerial, BeamSqlBackend)


unprocessedRuns
    :: (BeamSqlBackend be, HasSqlEqualityCheck be Run.Word32)
    => Q be LiquidityDb s (QGenExpr QValueContext be s (SqlSerial Run.Word32))
unprocessedRuns = do
    run  <- all_ $ runs liquidityDb
    path <- leftJoin_ (all_ $ pathQtys liquidityDb) (\path -> PathQty.pathQtyRun path `references_` run)
    guard_ $ isNothing_ path
    pure $ Run.runId run

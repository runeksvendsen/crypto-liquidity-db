{-# LANGUAGE GADTs #-}
module Query.Migrations
( addMigration
)
where

import Database ( liquidityDb, LiquidityDb(migrations) )
import Schema.Migration as Migration

import Database.Beam
import Database.Beam.Backend.SQL.SQL92
import Database.Beam.Backend.SQL (BeamSqlBackend,  BeamSqlBackendSyntax )
import Data.Maybe (fromMaybe)


addMigration ::
    ( MonadBeam be m
    , BeamSqlBackend be
    , HasSqlValueSyntax (Sql92ExpressionValueSyntax (Sql92SelectTableExpressionSyntax (Sql92SelectSelectTableSyntax (Sql92SelectSyntax (BeamSqlBackendSyntax be))))) Int16
    )
    => Int16
    -> m ()
addMigration fromVersion =
    runInsert $ insert (migrations liquidityDb) $
        insertExpressions [ Migration.Migration (val_ fromVersion) default_ (val_ False) ]

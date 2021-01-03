{-# LANGUAGE GADTs #-}
module Query.Migrations
( currentVersion
, addMigration
)
where

import Database ( liquidityDb, LiquidityDb(migrations) )
import Schema.Migration as Migration

import Database.Beam
import Database.Beam.Backend.SQL.SQL92
import Database.Beam.Backend.SQL (BeamSqlBackend,  BeamSqlBackendSyntax )
import Data.Maybe (fromMaybe)


currentVersion ::
    ( MonadBeam be f
    , FromBackendRow be Word16
    , HasQBuilder be
    , (HasSqlValueSyntax
        (Sql92ExpressionValueSyntax
            (Sql92SelectTableExpressionSyntax
            (Sql92SelectSelectTableSyntax
                (Sql92SelectSyntax
                    (Database.Beam.Backend.SQL.BeamSqlBackendSyntax be)))))
        Word16)
    )
    => f Word16
currentVersion = fmap (fromMaybe 0) $
    runSelectReturningOne $ select $
        limit_ 1 $
        orderBy_ desc_ $ do
        (+ 1) . Migration.migrationFromVersion <$> all_ (migrations liquidityDb)

addMigration ::
    ( MonadBeam be m
    , BeamSqlBackend be
    , HasSqlValueSyntax (Sql92ExpressionValueSyntax (Sql92SelectTableExpressionSyntax (Sql92SelectSelectTableSyntax (Sql92SelectSyntax (BeamSqlBackendSyntax be))))) Word16
    )
    => Word16
    -> m ()
addMigration fromVersion =
    runInsert $ insert (migrations liquidityDb) $
        insertExpressions [ Migration.Migration (val_ fromVersion) default_ ]

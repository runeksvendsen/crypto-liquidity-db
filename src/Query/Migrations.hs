{-# LANGUAGE GADTs #-}
module Query.Migrations
( currentVersion
)
where

import Database ( liquidityDb, LiquidityDb(migrations) )
import Schema.Migration as Migration
    ( Word16, MigrationT(migrationFromVersion) )

import Database.Beam
import Data.Maybe (fromMaybe)


currentVersion ::
    ( MonadBeam be f
    , FromBackendRow be Word16
    , HasQBuilder be
    )
    => f Word16
currentVersion = fmap (fromMaybe 0) $
    runSelectReturningOne $ select $
        limit_ 1 $
        orderBy_ desc_ $
        Migration.migrationFromVersion <$> all_ (migrations liquidityDb)

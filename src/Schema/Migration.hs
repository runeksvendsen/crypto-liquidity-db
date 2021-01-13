{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
module Schema.Migration
( MigrationT(..)
, Migration
, MigrationId
, PrimaryKey(type MigrationId)
  -- * Re-exports
, LocalTime
, Int16
)
where

import Internal.Prelude
import qualified Schema.Currency as Currency

import qualified Database.Beam              as Beam
import           Database.Beam              (C, Identity, PrimaryKey)
import Database.Beam.Backend.SQL.Types      (SqlSerial(unSerial))
import Data.Int                             (Int16)
import Data.Time.LocalTime                  (LocalTime)


-- |
data MigrationT f
    = Migration
    { migrationFromVersion :: C f Int16 -- ^ a migration from version @n@ to version @n+1@
    , migrationTime :: C f LocalTime -- ^ the time at which the migration was performed
    , migrationInProgress :: C f Bool
    } deriving Generic

type Migration = MigrationT Identity
type MigrationId = PrimaryKey MigrationT Identity

deriving instance Show Migration
deriving instance Eq Migration
instance Show MigrationId where
    show (MigrationId fromVersion) = "MigrationId " ++ show fromVersion
deriving instance Eq MigrationId

instance Beam.Beamable MigrationT

instance Beam.Table MigrationT where
    data PrimaryKey MigrationT f = MigrationId
        (C f Int16)
            deriving Generic
    primaryKey = MigrationId . migrationFromVersion

instance Beam.Beamable (PrimaryKey MigrationT)

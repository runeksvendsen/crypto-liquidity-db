{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
module Schema.Path
( PathT(..)
, Path
, PathId
, PrimaryKey(type PathId)
  -- * Re-exports
, UTCTime
, Int32
)
where

import Internal.Prelude
import qualified Schema.Currency as Currency

import qualified Database.Beam              as Beam
import           Database.Beam              (C, Identity, PrimaryKey)
import Data.Time.Clock                      (UTCTime)
import Database.Beam.Backend.SQL.Types      (SqlSerial(unSerial))
import Data.Int                            (Int16, Int32)


data PathT f
    = Path
    { pathId        :: C f (SqlSerial Int32)
    , pathPartCount :: C f Int16 -- ^ how many 'PathPart' are associated with this 'Path'?
    , pathStart     :: PrimaryKey Currency.CurrencyT f -- ^ where does the path start?
    } deriving Generic

type Path = PathT Identity
type PathId = PrimaryKey PathT Identity

deriving instance Show Path
deriving instance Eq Path
instance Show PathId where
    show (PathId serial) = "PathId " ++ show (unSerial serial)
deriving instance Eq PathId

instance Beam.Beamable PathT

instance Beam.Table PathT where
    data PrimaryKey PathT f = PathId
        (C f (SqlSerial Int32))
            deriving Generic
    primaryKey = PathId . pathId

instance Beam.Beamable (PrimaryKey PathT)

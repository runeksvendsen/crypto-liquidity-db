{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
module Schema.PathPart
( PathPartT(..)
, PathPart
, PathPartId
, PrimaryKey(type PathPartId)
, Int16
)
where

import Internal.Prelude

import qualified Schema.Path as Path
import qualified Schema.Currency as Currency
import qualified Schema.Venue as Venue

import qualified Database.Beam              as Beam
import           Database.Beam              (C, Identity, PrimaryKey)
import Data.Int (Int16)


data PathPartT f
    = PathPart
    { pathPartPath      :: PrimaryKey Path.PathT f
    , pathPartIndex     :: C f Int16
    , pathPartVenue     :: PrimaryKey Venue.VenueT f
    , pathPartCurrency  :: PrimaryKey Currency.CurrencyT f
    } deriving Generic

type PathPart = PathPartT Identity
type PathPartId = PrimaryKey PathPartT Identity

deriving instance Show PathPart
deriving instance Eq PathPart
instance Show PathPartId where
    show (PathPartId path index) =
        unwords ["PathPartId (", show path, ") ", show index]
deriving instance Eq PathPartId

instance Beam.Beamable PathPartT

instance Beam.Table PathPartT where
    data PrimaryKey PathPartT f = PathPartId
        (PrimaryKey Path.PathT f)
        (C f Int16)
            deriving Generic
    primaryKey PathPart{..} = PathPartId pathPartPath pathPartIndex

instance Beam.Beamable (PrimaryKey PathPartT)

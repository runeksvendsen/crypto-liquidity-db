{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
module Schema.Venue
( VenueT(..)
, Venue
, VenueId
, PrimaryKey(type VenueId)
)
where

import Internal.Prelude

import qualified Database.Beam              as Beam
import           Database.Beam              (C, Identity, PrimaryKey)
import qualified Data.Text as T
import Database.Beam.Backend.SQL.Types      (SqlSerial(unSerial))
import Data.Int (Int16)


data VenueT f
    = Venue
    { venueName :: C f T.Text
    } deriving Generic

type Venue = VenueT Identity
type VenueId = PrimaryKey VenueT Identity

deriving instance Show Venue
deriving instance Eq Venue
instance Show VenueId where
    show (VenueId venue) = toS venue
deriving instance Eq VenueId

instance Beam.Beamable VenueT

instance Beam.Table VenueT where
    data PrimaryKey VenueT f = VenueId
        (C f T.Text)
            deriving Generic
    primaryKey = VenueId . venueName

instance Beam.Beamable (PrimaryKey VenueT)

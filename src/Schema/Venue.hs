{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ExplicitNamespaces #-}
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
    { venueId   :: C f (SqlSerial Int16) -- supports ~32000 venues
    , venueName :: C f T.Text
    } deriving Generic

type Venue = VenueT Identity
type VenueId = PrimaryKey VenueT Identity

deriving instance Show Venue
deriving instance Eq Venue
instance Show VenueId where
    show (VenueId serial) = "VenueId" ++ show (unSerial serial)
deriving instance Eq VenueId

instance Beam.Beamable VenueT

instance Beam.Table VenueT where
    data PrimaryKey VenueT f = VenueId
        (C f (SqlSerial Int16))
            deriving Generic
    primaryKey = VenueId . venueId

instance Beam.Beamable (PrimaryKey VenueT)

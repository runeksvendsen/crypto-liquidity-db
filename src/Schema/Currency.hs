{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ExplicitNamespaces #-}
module Schema.Currency
( CurrencyT(..)
, Currency
, CurrencyId
, PrimaryKey(type CurrencyId)
)
where

import Internal.Prelude

import qualified Database.Beam              as Beam
import Database.Beam (C, Identity, PrimaryKey)
import qualified Data.Text as T
import Database.Beam.Backend.SQL.Types (SqlSerial(unSerial))
import Data.Int (Int32)


data CurrencyT f
    = Currency
    { currencyId     :: C f (SqlSerial Int32)
    , currencyName   :: C f T.Text
    , currencySymbol :: C f T.Text
    } deriving Generic

type Currency = CurrencyT Identity
type CurrencyId = PrimaryKey CurrencyT Identity

deriving instance Show Currency
deriving instance Eq Currency
instance Show CurrencyId where
    show (CurrencyId serial) = "CurrencyId " ++ show (unSerial serial)
deriving instance Eq CurrencyId

instance Beam.Beamable CurrencyT

instance Beam.Table CurrencyT where
    data PrimaryKey CurrencyT f = CurrencyId
        (C f (SqlSerial Int32))
            deriving Generic
    primaryKey = CurrencyId . currencyId

instance Beam.Beamable (PrimaryKey CurrencyT)

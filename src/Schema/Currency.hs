{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
module Schema.Currency
( CurrencyT(..)
, Currency
, CurrencyId
, PrimaryKey(type CurrencyId)
, Int32
)
where

import Internal.Prelude

import qualified Database.Beam              as Beam
import Database.Beam (C, Identity, PrimaryKey)
import qualified Data.Text as T
import Data.Int (Int32)


data CurrencyT f
    = Currency
    { currencySymbol :: C f T.Text
    } deriving Generic

type Currency = CurrencyT Identity
type CurrencyId = PrimaryKey CurrencyT Identity

deriving instance Show Currency
deriving instance Eq Currency
instance Show CurrencyId where
    show (CurrencyId symbol) = toS symbol
deriving instance Eq CurrencyId

instance Beam.Beamable CurrencyT

instance Beam.Table CurrencyT where
    data PrimaryKey CurrencyT f = CurrencyId
        (C f T.Text)
            deriving Generic
    primaryKey = CurrencyId . currencySymbol

instance Beam.Beamable (PrimaryKey CurrencyT)

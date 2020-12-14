{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
module Schema.RunCurrency
( RunCurrencyT(..)
, RunCurrency
, RunCurrencyId
, PrimaryKey(type RunCurrencyId)
)
where

import Internal.Prelude

import qualified CryptoDepth.OrderBook.Db.Schema.Run as Run
import qualified Schema.Path as Path
import qualified Schema.Currency as Currency

import qualified Database.Beam              as Beam
import           Database.Beam              (C, Identity, PrimaryKey)


-- | A currency that is present in one or more order books associated with a run
data RunCurrencyT f
    = RunCurrency
    { runCurrencyRun :: PrimaryKey Run.RunT f
    , runCurrencyCurrency :: PrimaryKey Currency.CurrencyT f
    } deriving Generic

type RunCurrency = RunCurrencyT Identity
type RunCurrencyId = PrimaryKey RunCurrencyT Identity

deriving instance Show RunCurrency
deriving instance Eq RunCurrency
instance Show RunCurrencyId where
    show (RunCurrencyId path index) =
        unwords ["RunCurrencyId (", show path, ") ", show index]
deriving instance Eq RunCurrencyId

instance Beam.Beamable RunCurrencyT

instance Beam.Table RunCurrencyT where
    data PrimaryKey RunCurrencyT f = RunCurrencyId
        (PrimaryKey Run.RunT f)
        (PrimaryKey Currency.CurrencyT f)
            deriving Generic
    primaryKey RunCurrency{..} = RunCurrencyId runCurrencyRun runCurrencyCurrency

instance Beam.Beamable (PrimaryKey RunCurrencyT)

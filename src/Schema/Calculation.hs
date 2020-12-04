{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
module Schema.Calculation
( CalculationT(..)
, Calculation
, CalculationId
, PrimaryKey(type CalculationId)
)
where

import Internal.Prelude

import qualified Schema.Currency as Currency
import qualified CryptoDepth.OrderBook.Db.Schema.Run as Run

import qualified Database.Beam              as Beam
import           Database.Beam              (C, Identity, PrimaryKey)
import Data.Time.Clock (UTCTime)


data CalculationT f
    = Calculation
    { calculationRun :: PrimaryKey Run.RunT f
    , calculationCurrency :: PrimaryKey Currency.CurrencyT f
    , calculationNumeraire :: PrimaryKey Currency.CurrencyT f
    , calculationSlippage :: C f Double
    , calculationStartTime :: C f UTCTime
    , calculationDurationSeconds :: C (Beam.Nullable f) Float -- null = "in progress" / non-null = "done"
    } deriving Generic

type Calculation = CalculationT Identity
type CalculationId = PrimaryKey CalculationT Identity

deriving instance Show Calculation
deriving instance Eq Calculation
instance Show CalculationId where
    show (CalculationId run currency numeraire slippage) =
        let paren s = "(" <> s <> ")"
        in unwords
            [ "CalculationId"
            , paren (show run)
            , paren (show currency)
            , paren (show numeraire)
            , paren (show slippage)
            ]
deriving instance Eq CalculationId

instance Beam.Beamable CalculationT

instance Beam.Table CalculationT where
    data PrimaryKey CalculationT f = CalculationId
        (PrimaryKey Run.RunT f)
        (PrimaryKey Currency.CurrencyT f)
        (PrimaryKey Currency.CurrencyT f)
        (C f Double)
            deriving Generic
    primaryKey Calculation{..} = CalculationId
        calculationRun calculationCurrency calculationNumeraire calculationSlippage

instance Beam.Beamable (PrimaryKey CalculationT)

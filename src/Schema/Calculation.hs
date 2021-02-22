{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
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
, new
, Int32
, Int64
, UTCTime
, fromCalcId
  -- * Re-exports
, Run.RunId
, getRunId
, mkRunId
)
where

import Internal.Prelude

import qualified Schema.Currency as Currency
import qualified Schema.RunCurrency as RC
import qualified Schema.CalculationParameter as CalcParam
import qualified CryptoDepth.OrderBook.Db.Schema.Run as Run

import qualified Database.Beam              as Beam
import           Database.Beam              (C, Identity, PrimaryKey)
import Database.Beam.Backend (SqlSerial(unSerial, SqlSerial))
import Data.Time (UTCTime)
import Data.Int (Int32)
import GHC.Int (Int64)


data CalculationT f
    = Calculation
    { calculationId :: C f (SqlSerial Int32)
    , calculationRun :: PrimaryKey Run.RunT f
    , calculationCurrency :: PrimaryKey Currency.CurrencyT f
    , calculationNumeraire :: PrimaryKey Currency.CurrencyT f
    , calculationSlippage :: C f Double
    -- sum of "path_qtys.qty" where path_qtys.calc__id = id.
    -- used to speed up SUM over path_qtys for calculation.
    -- must be updated for every INSERT into path_qtys.
    , calculationSumQty :: C (Beam.Nullable f) Int64
    , calculationCreationTime :: C f UTCTime
    , calculationStartTime :: C (Beam.Nullable f) UTCTime -- null = "not started", non-null = "in progress"
    , calculationDurationSeconds :: C (Beam.Nullable f) Double -- non-null = "done"
    } deriving Generic

type Calculation = CalculationT Identity
type CalculationId = PrimaryKey CalculationT Identity

new now rc cp = Calculation
    { calculationId = Beam.default_
    , calculationRun = Beam.val_ $ RC.rcRun rc
    , calculationCurrency = Beam.val_ $ RC.rcCurrency rc
    , calculationNumeraire = Beam.val_ $ CalcParam.cpNumeraire cp
    , calculationSlippage = Beam.val_ $ CalcParam.cpSlippage cp
    , calculationSumQty = Beam.val_ (-1)
    , calculationCreationTime = Beam.val_ now -- Beam.currentTimestamp_
    , calculationStartTime = Beam.val_ Nothing
    , calculationDurationSeconds = Beam.val_ Nothing
    }

deriving instance Show Calculation
deriving instance Eq Calculation
instance Show CalculationId where
    show (CalculationId num) = "CalculationId " ++ show num

deriving instance Eq CalculationId

fromCalcId :: CalculationId -> Int32
fromCalcId (CalculationId serial) = unSerial serial

instance Beam.Beamable CalculationT

instance Beam.Table CalculationT where
    data PrimaryKey CalculationT f = CalculationId
        (C f (SqlSerial Int32))
            deriving Generic
    primaryKey Calculation{..} = CalculationId
        calculationId

instance Beam.Beamable (PrimaryKey CalculationT)

getRunId :: Calculation -> Run.RunId
getRunId =
    calculationRun

mkRunId :: Int32 -> Run.RunId
mkRunId =
    Run.RunId . SqlSerial

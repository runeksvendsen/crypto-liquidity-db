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
, LocalTime
  -- * Re-exports
, Run.RunId
, getRunId
)
where

import Internal.Prelude

import qualified Schema.Currency as Currency
import qualified Schema.RunCurrency as RC
import qualified Schema.CalculationParameter as CalcParam
import qualified CryptoDepth.OrderBook.Db.Schema.Run as Run

import qualified Database.Beam              as Beam
import           Database.Beam              (C, Identity, PrimaryKey)
import Database.Beam.Backend (SqlSerial(SqlSerial))
import Data.Time.LocalTime (LocalTime)
import Data.Int (Int32)


data CalculationT f
    = Calculation
    { calculationId :: C f (SqlSerial Int32)
    , calculationRun :: PrimaryKey Run.RunT f
    , calculationCurrency :: PrimaryKey Currency.CurrencyT f
    , calculationNumeraire :: PrimaryKey Currency.CurrencyT f
    , calculationSlippage :: C f Double
    , calculationCreationTime :: C f LocalTime
    , calculationStartTime :: C (Beam.Nullable f) LocalTime -- null = "not started", non-null = "in progress"
    , calculationDurationSeconds :: C (Beam.Nullable f) Double -- non-null = "done"
    } deriving Generic

type Calculation = CalculationT Identity
type CalculationId = PrimaryKey CalculationT Identity

new rc cp = Calculation
    { calculationId = Beam.default_
    , calculationRun = Beam.val_ $ RC.runCurrencyRun rc
    , calculationCurrency = Beam.val_ $ RC.runCurrencyCurrency rc
    , calculationNumeraire = Beam.val_ $ CalcParam.calcParamNumeraire cp
    , calculationSlippage = Beam.val_ $ CalcParam.calcParamSlippage cp
    , calculationCreationTime = Beam.currentTimestamp_
    , calculationStartTime = Beam.val_ Nothing
    , calculationDurationSeconds = Beam.val_ Nothing
    }

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

getRunId :: Calculation -> Run.RunId
getRunId calc =
    let (CalculationId runId _ _ _) = Beam.pk calc
    in runId

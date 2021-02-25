{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
module Schema.PathSum
( PathSumT(..)
, PathSum
, PathSumId
, PrimaryKey(type PathSumId)
, Int64
)
where

import Internal.Prelude
import qualified Schema.Calculation as Calc

import qualified Database.Beam              as Beam
import           Database.Beam              (C, Identity, PrimaryKey)
import Database.Beam.Backend (SqlSerial(unSerial, SqlSerial))
import Data.Time (UTCTime)
import Data.Int (Int32, Int64)


-- | Speed up SUM over path_qtys.qty for a calculation
data PathSumT f
    = PathSum
    { pathsumCalc :: PrimaryKey Calc.CalculationT f
    , pathsumBuyQty :: C f Int64 -- ^ Sum of "buy" path_qtys.qty
    , pathsumSellQty :: C f Int64 -- ^ Sum of "sell" path_qtys.qty
    } deriving Generic

type PathSum = PathSumT Identity
type PathSumId = PrimaryKey PathSumT Identity

deriving instance Show PathSum
deriving instance Eq PathSum
instance Show PathSumId where
    show (PathSumId calc) =
        unwords ["PathSumId", "(" ++ show calc ++ ")"]
deriving instance Eq PathSumId

instance Beam.Beamable PathSumT

instance Beam.Table PathSumT where
    data PrimaryKey PathSumT f = PathSumId
        (PrimaryKey Calc.CalculationT f)
            deriving Generic
    primaryKey PathSum{..} = PathSumId pathsumCalc

instance Beam.Beamable (PrimaryKey PathSumT)

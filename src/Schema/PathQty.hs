{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
module Schema.PathQty
( PathQtyT(..)
, PathQty
, PathQtyId
, PrimaryKey(type PathQtyId)
)
where

import Internal.Prelude

import qualified Schema.Path as Path
import qualified Schema.Calculation as Calc

import qualified Database.Beam              as Beam
import           Database.Beam              (C, Identity, PrimaryKey)


data PathQtyT f
    = PathQty
    { pathQtyCalc       :: PrimaryKey Calc.CalculationT f
    , pathQtyPath       :: PrimaryKey Path.PathT f
    , pathQtyQty        :: C f Integer  -- TODO: precise enough?
    , pathQtyPriceLow   :: C f Double
    , pathQtyPriceHigh  :: C f Double
    } deriving Generic


type PathQty = PathQtyT Identity
type PathQtyId = PrimaryKey PathQtyT Identity

deriving instance Show PathQty
deriving instance Eq PathQty
instance Show PathQtyId where
    show (PathQtyId run path) =
        unwords ["PathQtyId (", show run, ") (", show path, ")"]
deriving instance Eq PathQtyId

instance Beam.Beamable PathQtyT

instance Beam.Table PathQtyT where
    data PrimaryKey PathQtyT f = PathQtyId
        (PrimaryKey Calc.CalculationT f)
        (PrimaryKey Path.PathT f)
            deriving Generic
    primaryKey PathQty{..} = PathQtyId pathQtyCalc pathQtyPath

instance Beam.Beamable (PrimaryKey PathQtyT)

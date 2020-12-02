{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ExplicitNamespaces #-}
module Schema.PathQty
( PathQtyT(..)
, PathQty
, PathQtyId
, PrimaryKey(type PathQtyId)
)
where

import Internal.Prelude

import qualified Schema.Path as Path
import qualified CryptoDepth.OrderBook.Db.Schema.Run as Run

import qualified Database.Beam              as Beam
import           Database.Beam              (C, Identity, PrimaryKey)


data PathQtyT f
    = PathQty
    { pathQtyRun        :: PrimaryKey Run.RunT f
    , pathQtyPath       :: PrimaryKey Path.PathT f
    , pathQtySlippage   :: C f Double
    , pathQtyQty        :: C f Integer  -- TODO: precise enough?
    , pathQtyPriceLow   :: C f Double
    , pathQtyPriceHigh  :: C f Double
    } deriving Generic


type PathQty = PathQtyT Identity
type PathQtyId = PrimaryKey PathQtyT Identity

deriving instance Show PathQty
deriving instance Eq PathQty
instance Show PathQtyId where
    show (PathQtyId run path slippage) =
        unwords ["PathQtyId (", show run, ") (", show path, ") ", show slippage]
deriving instance Eq PathQtyId

instance Beam.Beamable PathQtyT

instance Beam.Table PathQtyT where
    data PrimaryKey PathQtyT f = PathQtyId
        (PrimaryKey Run.RunT f)
        (PrimaryKey Path.PathT f)
        (C f Double)
            deriving Generic
    primaryKey PathQty{..} = PathQtyId pathQtyRun pathQtyPath pathQtySlippage

instance Beam.Beamable (PrimaryKey PathQtyT)

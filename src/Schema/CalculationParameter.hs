{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
module Schema.CalculationParameter
( CalcParamT(..)
, CalcParam
, CalcParamId
, PrimaryKey(type CalcParamId)
)
where

import Internal.Prelude

import qualified Schema.Currency as Currency

import qualified Database.Beam              as Beam
import           Database.Beam              (C, Identity, PrimaryKey)


data CalcParamT f
    = CalcParam
    { cpNumeraire :: PrimaryKey Currency.CurrencyT f
    , cpSlippage :: C f Double
    } deriving Generic

type CalcParam = CalcParamT Identity
type CalcParamId = PrimaryKey CalcParamT Identity

deriving instance Show CalcParam
deriving instance Eq CalcParam
instance Show CalcParamId where
    show (CalcParamId numeraire slippage) =
        let paren s = "(" <> s <> ")"
        in unwords
            [ "CalcParamId"
            , paren (show numeraire)
            , paren (show slippage)
            ]
deriving instance Eq CalcParamId

instance Beam.Beamable CalcParamT

instance Beam.Table CalcParamT where
    data PrimaryKey CalcParamT f = CalcParamId
        (PrimaryKey Currency.CurrencyT f)
        (C f Double)
            deriving Generic
    primaryKey CalcParam{..} = CalcParamId cpNumeraire cpSlippage

instance Beam.Beamable (PrimaryKey CalcParamT)

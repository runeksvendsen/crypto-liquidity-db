{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Database
( LiquidityDb(..)
, liquidityDb
, calcsForRun
, qtysForCalc
, pathForPathQty
, sumForCalc
)
where

import Internal.Prelude
import qualified CryptoDepth.OrderBook.Db.Schema.Run    as Run
import qualified CryptoDepth.OrderBook.Db.Schema.Order  as Order
import qualified CryptoDepth.OrderBook.Db.Schema.Book   as Book
import qualified Schema.RunCurrency as RC
import qualified Schema.Calculation as Calculation
import qualified Schema.PathSum
import qualified Schema.CalculationParameter as CalcParam
import qualified Schema.PathQty as PathQty
import qualified Schema.PathSum as PathSum
import qualified Schema.Path as Path
import qualified Schema.Venue as Venue
import qualified Schema.Currency as Currency
import qualified Schema.Migration as Migration

import qualified Database.Beam                          as Beam


-- |
data LiquidityDb f = LiquidityDb
    { -- Managed by 'crypto-orderbook-db'
      runs :: f (Beam.TableEntity Run.RunT)
    , orders :: f (Beam.TableEntity Order.OrderT)
    , books :: f (Beam.TableEntity Book.BookT)

      -- Input data and job queue for calculation
    , run_currencys :: f (Beam.TableEntity RC.RunCurrencyT)
    , calculation_parameters :: f (Beam.TableEntity CalcParam.CalcParamT)
    , calculations :: f (Beam.TableEntity Calculation.CalculationT)

      -- Calculation output data
    , paths :: f (Beam.TableEntity Path.PathT)
    , path_qtys :: f (Beam.TableEntity PathQty.PathQtyT)
    , path_sums :: f (Beam.TableEntity Schema.PathSum.PathSumT)

    , venues :: f (Beam.TableEntity Venue.VenueT)
    , currencys :: f (Beam.TableEntity Currency.CurrencyT)

      -- app metadata
    , migrations :: f (Beam.TableEntity Migration.MigrationT)
    } deriving Generic

instance Beam.Database be LiquidityDb

liquidityDb :: Beam.DatabaseSettings be LiquidityDb
liquidityDb = Beam.defaultDbSettings

calcsForRun
    :: Beam.HasSqlEqualityCheck be Currency.Int32
    => Run.RunT (Beam.QExpr be s)
    -> Beam.Q be LiquidityDb s (Calculation.CalculationT (Beam.QExpr be s))
calcsForRun =
    Beam.oneToMany_ (calculations liquidityDb)
                    Calculation.calculationRun

qtysForCalc
    :: Beam.HasSqlEqualityCheck be Currency.Int32
    => Calculation.CalculationT (Beam.QExpr be s)
    -> Beam.Q be LiquidityDb s (PathQty.PathQtyT (Beam.QExpr be s))
qtysForCalc =
    Beam.oneToMany_ (path_qtys liquidityDb)
                    PathQty.pathqtyCalc


pathForPathQty
    :: Beam.HasSqlEqualityCheck be Currency.Int32
    => Path.PathT (Beam.QExpr be s)
    -> Beam.Q be LiquidityDb s (PathQty.PathQtyT (Beam.QExpr be s))
pathForPathQty =
    Beam.oneToOne_ (path_qtys liquidityDb)
                    PathQty.pathqtyPath

sumForCalc
    :: Beam.HasSqlEqualityCheck be Currency.Int32
    => Calculation.CalculationT (Beam.QExpr be s)
    -> Beam.Q be LiquidityDb s (PathSum.PathSumT (Beam.QExpr be s))
sumForCalc =
    Beam.oneToOne_ (path_sums liquidityDb)
                    PathSum.pathsumCalc

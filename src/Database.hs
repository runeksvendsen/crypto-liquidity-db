{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Database
( LiquidityDb(..)
, liquidityDb
, partsForPath
)
where

import Internal.Prelude
import qualified CryptoDepth.OrderBook.Db.Schema.Run    as Run
import qualified CryptoDepth.OrderBook.Db.Schema.Order  as Order
import qualified CryptoDepth.OrderBook.Db.Schema.Book   as Book
import qualified Schema.RunCurrency as RC
import qualified Schema.Calculation as Calculation
import qualified Schema.CalculationParameter as CalcParam
import qualified Schema.PathQty as PathQty
import qualified Schema.PathPart as PathPart
import qualified Schema.Path as Path
import qualified Schema.Venue as Venue
import qualified Schema.Currency as Currency

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
    , path_parts :: f (Beam.TableEntity PathPart.PathPartT)
    , path_qtys :: f (Beam.TableEntity PathQty.PathQtyT)

    , venues :: f (Beam.TableEntity Venue.VenueT)
    , currencys :: f (Beam.TableEntity Currency.CurrencyT)
    } deriving Generic

instance Beam.Database be LiquidityDb

liquidityDb :: Beam.DatabaseSettings be LiquidityDb
liquidityDb = Beam.defaultDbSettings

partsForPath
    :: Beam.HasSqlEqualityCheck be Path.Word32
    => Path.PathT (Beam.QExpr be s)
    -> Beam.Q be LiquidityDb s (PathPart.PathPartT (Beam.QExpr be s))
partsForPath =
    Beam.oneToMany_ (path_parts liquidityDb)
                    PathPart.pathpartPath

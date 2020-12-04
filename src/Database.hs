{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Database
( LiquidityDb(..)
, liquidityDb
)
where

import Internal.Prelude
import qualified CryptoDepth.OrderBook.Db.Schema.Run    as Run
import qualified CryptoDepth.OrderBook.Db.Schema.Book   as Book
import qualified Schema.RunCurrency as RC
import qualified Schema.Calculation as Calculation
import qualified Schema.CalculationParameter as CalcParam
import qualified Schema.PathQty as PathQty
import qualified Schema.Path as Path
import qualified Schema.Venue as Venue
import qualified Schema.Currency as Currency

import qualified Database.Beam                          as Beam


-- |
data LiquidityDb f = LiquidityDb
    { -- Managed by 'crypto-orderbook-db'
      runs :: f (Beam.TableEntity Run.RunT)
    , books :: f (Beam.TableEntity Book.BookT)

      -- Input data and job queue for calculation
    , runCurrencies :: f (Beam.TableEntity RC.RunCurrencyT)
    , calculationParameters :: f (Beam.TableEntity CalcParam.CalcParamT)
    , calculations :: f (Beam.TableEntity Calculation.CalculationT)

      -- Calculation output data
    , paths :: f (Beam.TableEntity Path.PathT)
    , pathQtys :: f (Beam.TableEntity PathQty.PathQtyT)

      -- Map text to integer
    , venues :: f (Beam.TableEntity Venue.VenueT)
    , currencies :: f (Beam.TableEntity Currency.CurrencyT)
    } deriving Generic

instance Beam.Database be LiquidityDb

liquidityDb :: Beam.DatabaseSettings be LiquidityDb
liquidityDb = Beam.defaultDbSettings

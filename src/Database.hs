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
import qualified Schema.PathQty as PathQty
import qualified Schema.Path as Path
import qualified Schema.Venue as Venue
import qualified Schema.Currency as Currency

import qualified Database.Beam                          as Beam


-- |
data LiquidityDb f = LiquidityDb
    { runs :: f (Beam.TableEntity Run.RunT) -- ^ managed by 'crypto-orderbook-db'. only here to allow foreign key references
    , paths :: f (Beam.TableEntity Path.PathT)
    , pathQtys :: f (Beam.TableEntity PathQty.PathQtyT)
    , venues :: f (Beam.TableEntity Venue.VenueT)
    , currencies :: f (Beam.TableEntity Currency.CurrencyT)
    } deriving Generic

instance Beam.Database be LiquidityDb

liquidityDb :: Beam.DatabaseSettings be LiquidityDb
liquidityDb = Beam.defaultDbSettings

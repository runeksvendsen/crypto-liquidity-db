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
    , runCurrencys :: f (Beam.TableEntity RC.RunCurrencyT)
    , calculationParameters :: f (Beam.TableEntity CalcParam.CalcParamT)
    , calculations :: f (Beam.TableEntity Calculation.CalculationT)

      -- Calculation output data
    , paths :: f (Beam.TableEntity Path.PathT)
    , pathParts :: f (Beam.TableEntity PathPart.PathPartT)
    , pathQtys :: f (Beam.TableEntity PathQty.PathQtyT)

      -- Map text to integer
    , venues :: f (Beam.TableEntity Venue.VenueT)
    , currencys :: f (Beam.TableEntity Currency.CurrencyT)
    } deriving Generic

instance Beam.Database be LiquidityDb

liquidityDb :: Beam.DatabaseSettings be LiquidityDb
liquidityDb = Beam.defaultDbSettings -- `Beam.withDbModification`
--     Beam.dbModification {
--         currencys = Beam.setEntityName "currencys" -- <>
--             -- modifyTableFields tableModification {
--             -- _addressLine1 = "address1",
--             -- _addressLine2 = "address2"
--             -- },
--         , runCurrencys = Beam.setEntityName "run_currencys"
--         -- _shoppingCartOrders = setEntityName "orders" <>
--         --                         modifyTableFields tableModification {
--         --                         _orderShippingInfo = ShippingInfoId "shipping_info__id"
--         --                         },
--         -- _shoppingCartShippingInfos = setEntityName "shipping_info" <>
--         --                             modifyTableFields tableModification {
--         --                                 _shippingInfoId = "id",
--         --                                 _shippingInfoCarrier = "carrier",
--         --                                 _shippingInfoTrackingNumber = "tracking_number"
--         --                             },
--         -- _shoppingCartLineItems = setEntityName "line_items"
--     }

partsForPath
    :: Beam.HasSqlEqualityCheck be Path.Word32
    => Path.PathT (Beam.QExpr be s)
    -> Beam.Q be LiquidityDb s (PathPart.PathPartT (Beam.QExpr be s))
partsForPath =
    Beam.oneToMany_ (pathParts liquidityDb)
                    PathPart.pathpartPath



-- lel = [InvalidTableConstraint
--             (ForeignKey
--                 "calculations_currency__symbol_fkey"
--                 (TableName {tableName = "currencys"})
--                 (fromList [(ColumnName {columnName = "currency__symbol"},ColumnName {columnName = "symbol"})])
--                 NoAction
--                 NoAction
--             )
--             (NotAllColumnsExist
--                 (TableName {tableName = "currencys"})
--                 (fromList [ColumnName {columnName = "symbol"}])
--                 (fromList [ColumnName {columnName = "currency__symbol"},ColumnName {columnName = "run__id"}])
--             )
--         ]

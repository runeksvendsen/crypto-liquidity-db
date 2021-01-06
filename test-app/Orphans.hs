{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Orphans where

-- crypto-liquidity-db
import qualified Schema.Currency as Lib
import qualified Query.Books as Lib
import qualified Schema.Calculation as LibCalc

-- crypto-orderbook-db
import qualified CryptoDepth.OrderBook.Db.Schema.Run as Run

import Servant ( FromHttpApiData )
import qualified Data.Aeson as Json
import qualified Data.Aeson.Casing as Casing
import qualified  Data.Aeson.Casing.Internal as Casing


-- Drop the first word (until first capital letter) and
--  then apply a casing function
dropFirstWord :: (String -> String) -> (String -> String)
dropFirstWord f = f . Casing.dropFPrefix

prefixOptions :: Json.Options
prefixOptions = Json.defaultOptions
    { Json.fieldLabelModifier = dropFirstWord Casing.snakeCase
    , Json.sumEncoding = Json.ObjectWithSingleField
    , Json.constructorTagModifier = Casing.snakeCase
    }

instance Json.ToJSON LibCalc.Calculation where
    toJSON = Json.genericToJSON prefixOptions

instance Json.ToJSON Lib.CurrencyId
instance Json.ToJSON Run.RunId

instance FromHttpApiData Run.RunId
--     parseUrlPiece = error "TODO"

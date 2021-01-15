{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
module App.Main.WebApi.Orphans where

import Internal.Prelude (foldM, toS)

-- crypto-liquidity-db
import qualified Schema.Currency as Lib
import qualified Query.Books as Lib
import qualified Schema.Calculation as LibCalc

-- crypto-orderbook-db
import qualified CryptoDepth.OrderBook.Db.Schema.Run as Run

import Servant ( FromHttpApiData(..) )
import qualified Data.Aeson as Json
import qualified Data.Aeson.Casing as Casing
import qualified  Data.Aeson.Casing.Internal as Casing
import OrderBook.Graph.Types (Currency)
import Database.Beam.Backend.SQL.Types (SqlSerial(SqlSerial))
import Text.Read (readMaybe)
import qualified Data.Text as T


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

instance Json.ToJSON Run.Run where
    toJSON = Json.genericToJSON prefixOptions

instance Json.ToJSON Lib.CurrencyId
instance Json.ToJSON Run.RunId
instance Json.ToJSON Currency where
    toJSON = Json.toJSON . (toS :: Currency -> T.Text)

instance Json.FromJSON LibCalc.Calculation where
    parseJSON = Json.genericParseJSON prefixOptions

instance Json.FromJSON Run.Run where
    parseJSON = Json.genericParseJSON prefixOptions

instance Json.FromJSON Lib.CurrencyId
instance Json.FromJSON Run.RunId

instance FromHttpApiData Run.RunId where
   parseUrlPiece txt =
        let mkRunId :: Run.Word32 -> Run.RunId
            mkRunId = Run.RunId . SqlSerial
            handleError =
                maybe (Left $ toS $ "failed to parse run ID from " ++ show txt)
                      (Right . mkRunId)
        in handleError . readMaybe . toS $ txt

instance FromHttpApiData [Currency] where
    parseUrlPiece txt =
        let stringList = T.split (== ',') txt
        in foldM
            (\accum currency ->
                maybe (Left $ toS $ "failed to parse currency: " ++ show currency)
                      (Right . (: accum))
                      (readMaybe $ toS currency)
            )
            []
            (map T.strip stringList)

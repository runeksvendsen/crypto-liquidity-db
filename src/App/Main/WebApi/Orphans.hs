{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module App.Main.WebApi.Orphans where

import Internal.Prelude

-- crypto-liquidity-db
import App.Util (prefixOptions)
import qualified Schema.Currency as Lib
import qualified Query.Books as Lib
import qualified Schema.Path as Lib
import qualified Schema.PathQty as Lib
import qualified Schema.Calculation as LibCalc

-- crypto-orderbook-db
import qualified CryptoDepth.OrderBook.Db.Schema.Run as Run

import Servant (ToHttpApiData(..), FromHttpApiData(..))
import qualified Data.Aeson as Json
import OrderBook.Graph.Types (Currency)
import Database.Beam.Backend.SQL.Types (SqlSerial(SqlSerial))
import Text.Read (readMaybe)
import qualified Data.Text as T


instance Json.ToJSON LibCalc.Calculation where
    toJSON = Json.genericToJSON prefixOptions

instance Json.ToJSON Run.Run where
    toJSON = Json.genericToJSON prefixOptions

instance Json.ToJSON Lib.PathQty where
    toJSON = Json.genericToJSON prefixOptions
instance Json.FromJSON Lib.PathQty where
    parseJSON = Json.genericParseJSON prefixOptions

instance Json.ToJSON Lib.Path where
    toJSON = Json.genericToJSON prefixOptions
instance Json.FromJSON Lib.Path where
    parseJSON = Json.genericParseJSON prefixOptions

instance Json.FromJSON LibCalc.CalculationId
instance Json.ToJSON LibCalc.CalculationId
instance Json.FromJSON Lib.PathId
instance Json.ToJSON Lib.PathId

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
        let mkRunId :: Run.Int32 -> Run.RunId
            mkRunId = Run.RunId . SqlSerial
            handleError =
                maybe (Left $ toS $ "failed to parse run ID from " ++ show txt)
                      (Right . mkRunId)
        in handleError . readMaybe . toS $ txt

instance FromHttpApiData Currency where
    parseUrlPiece txt =
        if not (T.null txt)
            then return $ toS txt
            else Left "failed to parse currency from empty string"

instance FromHttpApiData [Currency] where
    parseUrlPiece txt =
        let stringList = T.split (== ',') txt
        in mapM parseUrlPiece $ filter (not . T.null) $ map T.strip stringList


instance ToHttpApiData [Currency] where
    toUrlPiece [] = "all"
    toUrlPiece lst = T.intercalate "," (map (toS . show) lst)

instance ToHttpApiData Currency where
    toUrlPiece = toS

instance ToHttpApiData LibCalc.RunId where
    toUrlPiece (Run.RunId (SqlSerial num)) = toS (show num)

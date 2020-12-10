{-# LANGUAGE OverloadedStrings #-}
module App.Source
( parseByteString
, toByteString
, Source(..)
)
where

import qualified Data.ByteString as BS


data Source
    = Runs
    | RunCurrencies
    | Calculations
        deriving (Eq, Show)

parseByteString :: BS.ByteString -> Either String Source
parseByteString "runs" = Right Runs
parseByteString "run_currencies" = Right RunCurrencies
parseByteString "calculations" = Right Calculations
parseByteString other = Left $ "Unknown source: " ++ show other

toByteString :: Source -> BS.ByteString
toByteString Runs = "runs"
toByteString RunCurrencies = "run_currencies"
toByteString Calculations = "calculations"

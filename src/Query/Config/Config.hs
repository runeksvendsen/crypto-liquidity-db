{-# LANGUAGE OverloadedStrings #-}

module Query.Config.Config
( numeraires
, creditInstruments
)
where

import Internal.Prelude


numeraires :: [Text]
numeraires =
    [ "USD"
    , "EUR"
    , "GBP"
    , "JPY"
    , "AUD"
    , "CAD"
    , "CHF"
    , "CNY"
    , "HKD"
    , "NZD"
    , "SEK"
    , "KRW"
    , "SGD"
    , "NOK"
    , "MXN"
    , "INR"
    , "RUB"
    , "ZAR"
    , "TRY"
    ]

creditInstruments :: [Text]
creditInstruments =
    [ "USDT"
    , "TUSD"
    , "TSD"
    , "USDC"
    , "BUSD"
    , "UST"
    , "TGBP"
    , "TAUD"
    , "TCAD"
    , "THKD"
    , "PAX"
    , "WBTC"
    , "BTCB"
    , "DGX"
    , "GUSD"
    -- , "EOSDT" -- not redeemable
    ]

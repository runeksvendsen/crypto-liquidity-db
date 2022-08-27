module Main where

import Control.Monad
import Text.Printf

main = do
    let mkMigrationStrings pair = map (\base -> mkMigration base pair) [Base, Quote]
    forM_ (concat $ map mkMigrationStrings input) putStrLn

data Side = Base | Quote

instance Show Side where
    show Base = "base"
    show Quote = "quote"

mkMigration side (from, to) =
    -- "UPDATE books SET " <> show side <> " = '" <> to <> "' WHERE venue = 'bitfinex' AND " <> show side <> " = '" <> from <> "';"
    printf "UPDATE books SET %s = '%s' WHERE venue = 'bitfinex' AND UPPER(%s) = '%s';"
                         (show side) to                        (show side) from

-- UPPER

input =
    [ ("AAA", "TESTAAA")
    , ("ALG", "ALGO")
    , ("AMP", "AMPL")
    , ("AMPF0", "AMPLF0")
    , ("ATO", "ATOM")
    , ("B21X", "B21")
    , ("BBB", "TESTBBB")
    , ("DAT", "DATA")
    , ("DOG", "MDOGE")
    , ("DSH", "DASH")
    , ("EDO", "PNT")
    , ("ETH2P", "ETH2Pending")
    , ("ETH2R", "ETH2Rewards")
    , ("ETH2X", "ETH2")
    , ("EUS", "EURS")
    , ("EUT", "EURT")
    , ("GNT", "GLM")
    , ("IDX", "ID")
    , ("IOT", "IOTA")
    , ("LBT", "LBTC")
    , ("LES", "LEO-EOS")
    , ("LET", "LEO-ERC20")
    , ("LNX", "LN-BTC")
    , ("MNA", "MANA")
    , ("OMN", "OMNI")
    , ("PAS", "PASS")
    , ("PBTCEOS", "pBTC-EOS")
    , ("PBTCETH", "PBTC-ETH")
    , ("PETHEOS", "pETH-EOS")
    , ("PLTCEOS", "PLTC-EOS")
    , ("PLTCETH", "PLTC-ETH")
    , ("QSH", "QASH")
    , ("QTM", "QTUM")
    , ("RBT", "RBTC")
    , ("REP", "REP2")
    , ("SNG", "SNGLS")
    , ("STJ", "STORJ")
    , ("TSD", "TUSD")
    , ("UDC", "USDC")
    , ("USK", "USDK")
    , ("UST", "USDT")
    , ("VSY", "VSYS")
    , ("WBT", "WBTC")
    , ("XCH", "XCHF")
    , ("YGG", "YEED")
    , ("YYW", "YOYOW")
    ]
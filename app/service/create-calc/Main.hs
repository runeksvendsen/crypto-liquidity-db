{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified App.Main.CreateCalc
import qualified App.Main.Util
import App.Monad ( Config(..), CfgConstants(..), CfgParams(..) )


main :: IO ()
main =
    App.Main.Util.withDbPool
        App.Main.Util.LevelDebug
        (App.Main.CreateCalc.main . mkCfg)
 where
    mkCfg pool = Config
        { cfgConstants =
            CfgConstants
                { cfgParams = CfgParams
                    { cfgNumeraires = ["USD"]
                    , cfgSlippages = [0.01, 0.1, 0.5]
                    }
                , cfgMaxCalculationTime = 1800
                , cfgDeadMonitorInterval = 3600
                }
        , cfgDbConnPool = pool
        }

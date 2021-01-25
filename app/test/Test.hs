{-# LANGUAGE GADTs #-}
{-# LANGUAGE BangPatterns #-}
module Test
( runTest
, testCaseCalc
, testCaseLiquidity
, Spec
, testCaseBooks
)
where

import qualified Schema.Calculation as LibCalc
import qualified Query.Liquidity as Lib
import qualified Query.Books as Lib
import Data.Maybe (isJust)
import Data.List (all)


type Spec a =
    [   ( String   -- description
        , a -- expected
        , a -- actual
        , a -> a -> Bool -- comparison func: @f expected actual@
        )
    ]

runTest :: Show a => Spec a -> IO (Bool, String)
runTest = return .
    foldl runTest' (True, "")
  where
      runTest' (!successState, !msg) (descr, expected, actual, fun) =
          let success = fun expected actual
              failureMsg = unlines
                  [ "FAILURE: " ++ descr
                  , "Expected:\n" ++ show expected
                  , ""
                  , "Actual:\n" ++ show actual
                  , ""
                  ]
          in (successState && success, if success then msg else msg ++ failureMsg  )

testCaseBooks
    :: [Lib.OrderBook Double]
    -> Spec Int
testCaseBooks obs =
        [ ("non-empty order book list", 0, length obs, (/=))
        ]

testCaseLiquidity
    :: [Lib.LiquidityData]
    -> Spec [Lib.LiquidityData]
testCaseLiquidity ld =
        [ ("non-empty liquidity data", [], ld, (/=))
        ]

testCaseCalc
    :: [LibCalc.Calculation]
    -> Spec [LibCalc.Calculation]
testCaseCalc calcs =
        [ ("at least a single calculation", [], calcs, (/=))
        , ("no unfinished calculations", [], unfinishedCalculations calcs, (==))
        ]

unfinishedCalculations :: [LibCalc.Calculation] -> [LibCalc.Calculation]
unfinishedCalculations =
    filter (not . isFinishedCalculation)
  where
    isFinishedCalculation calc =
        isJust (LibCalc.calculationStartTime calc)
        && isJust (LibCalc.calculationDurationSeconds calc)

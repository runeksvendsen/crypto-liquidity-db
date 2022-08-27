{-# LANGUAGE GADTs #-}
{-# LANGUAGE BangPatterns #-}
module Test
( runTest
, testCaseCalc
, testCaseLiquidity
, Spec
, testCaseBooks
, testCaseBook
)
where

import qualified Schema.Calculation as LibCalc
import qualified Query.Liquidity as Lib
import qualified Query.Books as Lib
import Data.Maybe (isJust)
import Data.List (all)
import qualified Data.Text as T


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

testCaseBook
    :: ( Lib.BookResult (Lib.OrderBook Double) -- The book returned by the endpoint
       , Lib.OrderBook Double -- The book we want to fetch
       )
    -> Spec (Lib.BookResult (Lib.OrderBook Double))
testCaseBook (bs, ob) =
        [ ("non-null result", Lib.emptyBookResult, bs, \_ actual -> isJust $ Lib.result actual)
        , ("no warnings", Lib.emptyBookResult, bs, \_ actual -> null $ Lib.warnings actual)
        , ("BookResult matches expected book", Lib.emptyBookResult, bs, \_ actual -> Lib.result actual == Just ob)
        ] -- NB: we use 'Lib.emptyBookResult' and ignore it because the 'Spec' type is not generic enough

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

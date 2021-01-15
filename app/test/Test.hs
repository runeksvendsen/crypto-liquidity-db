{-# LANGUAGE GADTs #-}
{-# LANGUAGE BangPatterns #-}
module Test
( runTest
, testCase
)
where

import qualified Schema.Calculation as LibCalc
import Data.Maybe (isJust)
import Data.List (all)


type Spec =
    [   ( String   -- description
        , [LibCalc.Calculation] -- expected
        , [LibCalc.Calculation] -- actual
        , [LibCalc.Calculation] -> [LibCalc.Calculation] -> Bool -- comparison func: @f expected actual@
        )
    ]

runTest :: Spec -> IO (Bool, String)
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

testCase
    :: [LibCalc.Calculation]
    -> Spec
testCase calcs =
        [ ("at least a single calculation", [], calcs, (/=))
        , ("no unfinished calculations", [], calcs, (==))
        ]

unfinishedCalculations :: [LibCalc.Calculation] -> [LibCalc.Calculation]
unfinishedCalculations = filter (not . isFinishedCalculation)

isFinishedCalculation :: LibCalc.Calculation -> Bool
isFinishedCalculation calc =
    isJust (LibCalc.calculationStartTime calc)
    && isJust (LibCalc.calculationDurationSeconds calc)

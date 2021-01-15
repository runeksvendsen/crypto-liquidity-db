{-# LANGUAGE GADTs #-}
module Test
( runTest
, testCase
)
where

import qualified Schema.Calculation as LibCalc

import System.IO.Temp (withSystemTempFile)
import Test.Hspec.Contrib.HUnit (fromHUnitTest)
import Test.HUnit (Test(..))
import Test.Hspec (Spec, shouldNotBe, shouldBe, describe, it)
import Data.Maybe (isJust)
import Data.List (all)
import Test.Hspec.Runner (isSuccess, defaultConfig, Config(..), runSpec)
import GHC.IO.Handle (hGetContents)


runTest :: Spec -> IO (Bool, String)
runTest spec =
    withSystemTempFile "hspec-output" $ \_ hdl -> do
        summary <- runSpec spec (mkCfg hdl)
        testOutput <- hGetContents hdl
        return (isSuccess summary, testOutput)
  where
    mkCfg hdl = defaultConfig { configOutputFile = Left hdl }

testCase :: [LibCalc.Calculation] -> Spec
testCase calcs =
    describe "after processing" $ do
        it "at least a single calculation" $
            calcs `shouldNotBe` []
        it "no unfinished calculations" $
            unfinishedCalculations calcs `shouldBe` []

unfinishedCalculations :: [LibCalc.Calculation] -> [LibCalc.Calculation]
unfinishedCalculations = filter (not . isFinishedCalculation)

isFinishedCalculation :: LibCalc.Calculation -> Bool
isFinishedCalculation calc =
    isJust (LibCalc.calculationStartTime calc)
    && isJust (LibCalc.calculationDurationSeconds calc)

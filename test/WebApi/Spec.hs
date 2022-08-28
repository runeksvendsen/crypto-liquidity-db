{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module WebApi.Spec
( spec
)
where

-- crypto-liquidity-db
import Internal.Prelude
import qualified App.Main.WebApi
import qualified App.Monad as AppLib
import qualified App.Main.Util
import qualified App.Pool
import qualified App.Log
import App.Monad ( Config(..), CfgConstants(..), CfgParams(..) )

-- crypto-liquidity-db
import qualified Database as Lib
import qualified Schema.Currency as Lib
import qualified Query.Liquidity as Lib
import qualified Query.Books as Lib
import qualified Query.Calculations as Lib
import qualified Schema.Calculation as LibCalc
import qualified OrderBook.Graph as Lib

import Control.Monad.IO.Class
import Servant
import System.Environment (getArgs, lookupEnv)
import Data.Maybe (isJust, fromMaybe)
import qualified Servant.Client as SC
import qualified Network.HTTP.Client as HTTP
import Data.List (intercalate)
import Control.Exception (assert)
import Data.Tuple (swap)
import qualified Test.Hspec as Hspec
import Servant.Client (ClientEnv)
import Test.Hspec.Expectations.Pretty (shouldBe, shouldNotBe, shouldSatisfy)
import qualified Test.QuickCheck as QC

countTestCaseBook :: Int
countTestCaseBook = 100

spec :: ClientEnv -> Hspec.Spec
spec env = do
    Hspec.describe "WebApi" $ do
        Hspec.describe "allCalculations" $ do
            calcs <- Hspec.runIO $ runCM allCalculations
            Hspec.it "at least a single calculation" $ do
                calcs `shouldNotBe` []
            Hspec.it "no unfinished calculations" $ do
                unfinishedCalculations calcs `shouldBe` []
        Hspec.describe "allLiquidity" $ do
            ld <- Hspec.runIO $ runCM allLiquidity'
            Hspec.it "non-empty liquidity data" $
                ld `shouldNotBe` []
        allBooks <- Hspec.runIO $ runCM (runBooks runId)
        Hspec.describe "testCaseBooks" $
            Hspec.it "non-empty order book list" $
                allBooks `shouldNotBe` []
        allBooksShuffled <- Hspec.runIO . QC.generate . QC.shuffle $ allBooks
        forM_ (take countTestCaseBook allBooksShuffled) fullTestCaseBook
  where
    runId = LibCalc.mkRunId 1
    allLiquidity' = allLiquidity "USD" 0.5 Nothing Nothing Nothing
    runBook' targetBook (currency1, currency2) = do
        res <- runBook runId (Lib.bookVenue targetBook) (toS currency1) (toS currency2)
        pure (res, targetBook)

    runCM :: SC.ClientM a -> IO a
    runCM action =
        SC.runClientM action env >>=
        either (fail . ("runClientM failed: " ++) . show) pure

    fullTestCaseBook targetBook = do
        Hspec.describe ("testCaseBook: " <> toS (Lib.showBook targetBook)) $ do
            let baseQuote = Lib.baseQuote targetBook
            Hspec.describe "(base, quote)" $ do
                res <- Hspec.runIO $ runCM $ runBook' targetBook baseQuote
                testCaseBook res
            Hspec.describe "(quote, base)" $ do
                res <- Hspec.runIO $ runCM $ runBook' targetBook (swap baseQuote)
                testCaseBook res

    testCaseBook (bs, ob) = Hspec.describe "BookResult" $ do
        Hspec.it "no warnings" $
            Lib.warnings bs `shouldBe` []
        Hspec.it "matches expected book" $
            (Lib.sortOrders <$> Lib.result bs) `shouldBe` Just (Lib.sortOrders ob)

    unfinishedCalculations :: [LibCalc.Calculation] -> [LibCalc.Calculation]
    unfinishedCalculations =
        filter (not . isFinishedCalculation)
        where
        isFinishedCalculation calc =
            isJust (LibCalc.calculationStartTime calc)
            && isJust (LibCalc.calculationDurationSeconds calc)

allCalculations :: SC.ClientM [LibCalc.Calculation]
allLiquidity
    :: App.Main.WebApi.Currency
    -> Double
    -> Maybe LibCalc.UTCTime
    -> Maybe LibCalc.UTCTime
    -> Maybe Word
    -> SC.ClientM [Lib.LiquidityData]
runBooks :: LibCalc.RunId -> SC.ClientM [Lib.OrderBook Double]
runBook
    :: LibCalc.RunId
    -> Text
    -> App.Main.WebApi.Currency
    -> App.Main.WebApi.Currency
    -> SC.ClientM (Lib.BookResult (Lib.OrderBook Double))
allLiquidity :<|> _ :<|> allCalculations :<|> _ :<|> _ :<|> runBooks :<|> runBook :<|> _ =
    SC.client api
  where
    api :: Proxy App.Main.WebApi.API
    api = Proxy

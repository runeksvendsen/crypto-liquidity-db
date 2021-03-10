module Process.Prop.Graph
( spec
)
where

import Internal.Prelude

import qualified Process.WebApiRead
import qualified Process.Spec

import qualified Query.Graph
import           Test.Hspec.Expectations.Pretty
import qualified Data.Vector as Vec
import qualified Test.Hspec as Hspec

import qualified Test.QuickCheck as QC
import Data.Maybe (fromMaybe)


spec :: Process.WebApiRead.ClientEnv -> Process.Spec.SetupDone -> Hspec.Spec
spec env _ = do
    Hspec.describe "NewestRunAllPaths" $ do
        Hspec.it "edge src/dst `elem` node index list" $ QC.property $ \limitM -> do
            graphDataM <- throwError <$> Process.WebApiRead.runPathAllReq
                env Process.Spec.numeraire Process.Spec.slippage limitM
            testAllLinks (fromMaybe (error "empty graph data") graphDataM)
  where
    testAllLinks graphData =
        forM_ (Query.Graph.links graphData) $ \link -> do
            let nodeIndices = map Query.Graph.index $ Vec.toList (Query.Graph.nodes graphData)
            nodeIndices `shouldContain` [Query.Graph.source link]
            nodeIndices `shouldContain` [Query.Graph.target link]

    throwError = either (error . show) id

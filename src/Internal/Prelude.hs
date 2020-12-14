module Internal.Prelude
( module Export
, toS
, groupOn
, for
, runInsertReturningOne
)
where

import GHC.Generics as Export (Generic)
import Data.Text as Export (Text)
import Control.Monad as Export
import Protolude.Conv (toS)
import Database.Beam.Backend.SQL.BeamExtensions (MonadBeamInsertReturning(runInsertReturningList))
import Data.List (groupBy, sortOn)


groupOn :: Ord b => (t -> b) -> [t] -> [[t]]
groupOn f = groupBy (\a1 a2 -> f a1 == f a2) . sortOn f

for :: [a] -> (a -> b) -> [b]
for = flip map

runInsertReturningOne insert = do
    lst <- runInsertReturningList insert
    case lst of
        [res] -> return res
        other -> error $ "Query did not return single result. Result: " ++ show other

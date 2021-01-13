{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Internal.Prelude
( module Export
, NominalDiffTime
, toS
, groupOn
, for
, runInsertReturningOne
, uniqueOn
)
where

import GHC.Generics as Export (Generic)
import Data.Text as Export (Text)
import Control.Monad as Export
import Protolude.Conv (toS)
import Database.Beam.Backend.SQL.BeamExtensions (MonadBeamInsertReturning(runInsertReturningList))
import Data.List (groupBy, sortOn)
import Data.Time.Clock (NominalDiffTime)


groupOn :: Ord b => (t -> b) -> [t] -> [[t]]
groupOn f = groupBy (\a1 a2 -> f a1 == f a2) . sortOn f

uniqueOn :: Ord b => (a -> b) -> [a] -> [a]
uniqueOn f = map head . groupOn f

for :: [a] -> (a -> b) -> [b]
for = flip map

runInsertReturningOne insert = do
    lst <- runInsertReturningList insert
    case lst of
        [res] -> return res
        other -> error $ "Query did not return single result. Result: " ++ show other

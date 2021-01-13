{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Internal.Prelude
( module Export
, NominalDiffTime
, bracket
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
import qualified Control.Exception as E


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

-- | Like 'Control.Exception.Base' except the "release function" takes
--    'Just' @c@ value if in-between computation does not throw exception, otherwise 'Nothing'.
bracket
        :: IO a         -- ^ computation to run first (\"acquire resource\")
        -> (Maybe c -> a -> IO b)  -- ^ computation to run last (\"release resource\")
        -> (a -> IO c)  -- ^ computation to run in-between
        -> IO c         -- returns the value from the in-between computation
bracket before after thing =
  E.mask $ \restore -> do
    a <- before
    c <- restore (thing a) `E.onException` after Nothing a
    _ <- after (Just c) a
    return c

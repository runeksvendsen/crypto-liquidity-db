{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
module Insert.Venues
( insertMissingVenues
)
where

import Database
-- import qualified Schema.Currency as Currency
import qualified Schema.Venue as Venue

import Database.Beam
import qualified Data.Set as Set
import Database.Beam.Backend.SQL.BeamExtensions             (runInsertReturningList)


-- TODO: generalize 'Query.Currencies'

insertMissingVenues lst = do
    existingCurrencyRows <- runSelectReturningList $ select $ lookupExistingVenues lst
    let existingCurrencies = Set.fromList $ map Venue.venueName existingCurrencyRows
        missingCurrencies = Set.fromList lst Set.\\ existingCurrencies
    runInsert $ insert (venues liquidityDb) $
        insertExpressions $ map (Venue.Venue . val_) (Set.toList missingCurrencies)

lookupExistingVenues [] = do
    venue <- all_ $ venues liquidityDb
    guard_ (val_ False)
    pure venue
lookupExistingVenues (first : rest) = do
    venue <- all_ $ venues liquidityDb
    guard_ $ foldr
        (\inCurr state -> state ||. Venue.venueName venue ==. val_ inCurr)
        (Venue.venueName venue ==. val_ first)
        rest
    pure venue

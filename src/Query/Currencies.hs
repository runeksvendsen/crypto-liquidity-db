{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
module Query.Currencies
( insertMissingCurrencies
)
where

import Database
import qualified Schema.Currency as Currency

import Database.Beam
import qualified Data.Set as Set
import Database.Beam.Backend.SQL.BeamExtensions             (runInsertReturningList)


insertMissingCurrencies lst = do
    existingCurrencyRows <- runSelectReturningList $ select $ lookupExistingCurrencies lst
    let existingCurrencies = Set.fromList $ map Currency.currencySymbol existingCurrencyRows
        missingCurrencies = Set.fromList lst Set.\\ existingCurrencies
    runInsert $ insert (currencys liquidityDb) $
        insertExpressions $ map (Currency.Currency . val_) (Set.toList missingCurrencies)

lookupExistingCurrencies [] = do
    currency <- all_ $ currencys liquidityDb
    guard_ (val_ False)
    pure currency
lookupExistingCurrencies (first : rest) = do
    currency <- all_ $ currencys liquidityDb
    guard_ $ foldr
        (\inCurr state -> state ||. Currency.currencySymbol currency ==. val_ inCurr)
        (Currency.currencySymbol currency ==. val_ first)
        rest
    pure currency

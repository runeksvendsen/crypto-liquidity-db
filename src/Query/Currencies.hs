{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
module Query.Currencies
( lookupOrInsertCurrencies
, lookupCurrency
)
where

import Database
import qualified Schema.Currency as Currency

import Database.Beam
import qualified Data.Set as Set
import Database.Beam.Backend.SQL.BeamExtensions             (runInsertReturningList)


lookupOrInsertCurrencies lst = do
    existingCurrencyRows <- runSelectReturningList $ select $ lookupExistingCurrencies lst
    let existingCurrencies = Set.fromList $ map Currency.currencySymbol existingCurrencyRows
        missingCurrencies = Set.fromList lst Set.\\ existingCurrencies
    insertedCurrencies <- runInsertReturningList $ insert (currencies liquidityDb) $
        insertExpressions $ map (Currency.Currency default_ . val_) (Set.toList missingCurrencies)
    return $ existingCurrencyRows ++ insertedCurrencies

lookupExistingCurrencies [] = do
    currency <- all_ $ currencies liquidityDb
    guard_ (val_ False)
    pure currency
lookupExistingCurrencies (first : rest) = do
    currency <- all_ $ currencies liquidityDb
    guard_ $ foldr
        (\inCurr state -> state ||. Currency.currencySymbol currency ==. val_ inCurr)
        (Currency.currencySymbol currency ==. val_ first)
        rest
    pure currency

lookupCurrency symbol = do
    currency <- all_ $ currencies liquidityDb
    guard_ $ Currency.currencySymbol currency ==. val_ symbol
    pure $ pk currency

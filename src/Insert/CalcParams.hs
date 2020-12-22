{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Insert.CalcParams
( setCalcParams
)
where

import           Internal.Prelude
import           Data.List                                ( sort )
import qualified Data.List.NonEmpty                       as NE

import           Database
import           Database.Beam
import           Database.Beam.Backend
import           Database.Beam.Backend.SQL.BeamExtensions

import qualified OrderBook.Graph                          as G

import qualified Schema.CalculationParameter as CP
import qualified Insert.Currencies as QC

import           Schema.Currency
                 ( PrimaryKey(CurrencyId) )
import qualified Schema.Path                              as Path
import qualified Schema.PathPart                          as PP
import qualified Schema.PathQty                           as PQty


-- | Delete existing, and insert the given, calculations parameters
setCalcParams cpList = do
    runDelete $ delete (calculation_parameters liquidityDb) (const $ val_ True)
    QC.insertMissingCurrencies (uniqueOn id $ map fst cpList)
    runInsert $ insert (calculation_parameters liquidityDb) $
        insertValues $ for cpList $ \(numeraire, slippage) ->
            CP.CalcParam
                { CP.cpNumeraire = CurrencyId numeraire
                , CP.cpSlippage = slippage
                }

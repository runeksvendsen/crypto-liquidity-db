{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Update.Calculation where

import           Data.List                                ( sort )
import qualified Data.List.NonEmpty                       as NE

import           Database
import           Database.Beam
import           Database.Beam.Backend
import           Database.Beam.Backend.SQL.BeamExtensions

import           Internal.Prelude

import qualified OrderBook.Graph                          as G

import qualified Schema.Calculation                       as Calc
import           Schema.Currency
                 ( PrimaryKey(CurrencyId) )
import qualified Schema.Path                              as Path
import qualified Schema.PathQty                           as PQty
import           Schema.Venue
                 ( PrimaryKey(VenueId) )
import Data.Int (Int16)


updateDuration calcPk durationSecs =
    runUpdate $ update
        (calculations liquidityDb)
        (\calc' -> Calc.calculationDurationSeconds calc' <-. just_ (val_ durationSecs))
        (\calc' -> pk calc' ==. val_ (calcPk :: Calc.CalculationId))

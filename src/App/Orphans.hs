{-# LANGUAGE MultiParamTypeClasses #-}
module App.Orphans where

import Database.Beam (FromBackendRow)
import qualified Database.Beam.Postgres as Pg


instance FromBackendRow Pg.Postgres Float

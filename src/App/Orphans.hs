{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module App.Orphans where

import Database.Beam (FromBackendRow(..))
import qualified Database.Beam.Postgres as Pg
import OrderBook.Graph.Types (Currency)
import Data.Text (Text)
import Internal.Prelude (toS)

instance FromBackendRow Pg.Postgres Currency where
    fromBackendRow = toS @Text <$> fromBackendRow

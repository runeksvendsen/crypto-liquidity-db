module App.Monad
( module App.Monad
, R.lift
)
where

import Internal.Prelude

import App.Orphans ()


import Data.Time.Clock (NominalDiffTime)
import qualified Database.Beam.Postgres as Pg
import Database.Beam.Backend.SQL.BeamExtensions (MonadBeamInsertReturning(runInsertReturningList))
import qualified Control.Monad.Reader as R



type AppM = R.ReaderT Config IO

logInfo :: String -> AppM ()
logInfo = R.lift . putStrLn

runAppM :: Config -> R.ReaderT Config m a -> m a
runAppM = flip R.runReaderT

withDbConn :: (Pg.Connection -> AppM a) -> AppM a
withDbConn f = do
    cfg <- R.ask
    let withConn f = error "TODO"
    withConn cfg

dbRun :: Pg.Pg a -> AppM a
dbRun appM = do
    withDbConn $ \conn -> R.lift $ Pg.runBeamPostgresDebug putStrLn conn appM

-- |
data Config = Config
    { cfgNumeraires :: [Text]
    , cfgSlippages :: [Double]
    , cfgMaxCalculationTime :: NominalDiffTime
      -- ^ the amount of time after which a calculation is considered stalled/dead
    , cfgDeadMonitorInterval :: NominalDiffTime
      -- ^ restart dead calculations this often
    }

module App.Monad
( module App.Monad
, R.lift
)
where

import Internal.Prelude

import App.Orphans ()


import Data.Time.Clock (NominalDiffTime)
import qualified Database.Beam.Postgres as Pg
import qualified Database.PostgreSQL.Simple as PgSimple
import qualified Control.Monad.Reader as R
import qualified App.Pool as Pool
import qualified App.Log as Log
import qualified Control.Concurrent.Async as Async


type AppM = R.ReaderT Config IO

logInfo :: String -> String -> AppM ()
logInfo ctx =
    R.lift . Log.logInfo (toS ctx)

logDebug :: String -> String -> AppM ()
logDebug ctx =
    R.lift . Log.logDebug (toS ctx)

runAppM :: Config -> R.ReaderT Config m a -> m a
runAppM = flip R.runReaderT

withDbConn :: (Pg.Connection -> AppM a) -> AppM a
withDbConn f = do
    pool <- R.asks cfgDbConnPool
    Pool.withResource pool f

dbRun :: Pg.Pg a -> AppM a
dbRun appM = do
    cfg <- R.ask
    withDbConn $ \conn -> R.lift $
        PgSimple.withTransaction conn $
            Pg.runBeamPostgresDebug (runAppM cfg . logDebug "SQL") conn appM

async :: AppM a -> AppM (Async.Async a)
async appM = do
    cfg <- R.ask
    R.lift $ Async.async $ runAppM cfg appM

-- |
data Config = Config
    { cfgNumeraires :: [Text]
    , cfgSlippages :: [Double]
    , cfgMaxCalculationTime :: NominalDiffTime
      -- ^ the amount of time after which a calculation is considered stalled/dead
    , cfgDeadMonitorInterval :: NominalDiffTime
      -- ^ restart dead calculations this often
    , cfgDbConnPool :: Pool.Pool Pool.Connection
    }

calculationParameters :: Config -> [(Text, Double)]
calculationParameters cfg = do
    numeraire <- cfgNumeraires cfg
    slippage <- cfgSlippages cfg
    return (numeraire, slippage)

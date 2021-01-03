{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module App.Monad
( module App.Monad
, R.lift
, R.ask
)
where

import Internal.Prelude

import App.Orphans ()


import Data.Time.Clock (NominalDiffTime)
import qualified Database.Beam.Postgres as Pg
import qualified Database.PostgreSQL.Simple as PgSimple
import qualified Database.PostgreSQL.Simple.Transaction as PgSimple
import qualified Control.Monad.Reader as R
import qualified App.Pool as Pool
import qualified App.Log as Log
import qualified Control.Concurrent.Async as Async
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Base (MonadBase)



type AppM = R.ReaderT Config IO

logInfo :: String -> String -> AppM ()
logInfo ctx =
    R.lift . Log.logInfo (toS ctx)

logDebug :: String -> String -> AppM ()
logDebug ctx =
    R.lift . Log.logDebug (toS ctx)

logError :: String -> String -> AppM ()
logError ctx =
    R.lift . Log.logError (toS ctx)

runAppM :: Config -> R.ReaderT Config m a -> m a
runAppM = flip R.runReaderT

withDbConn :: (Pg.Connection -> AppM a) -> AppM a
withDbConn f = do
    pool <- R.asks cfgDbConnPool
    Pool.withResource pool f

runBeamTx :: Pg.Pg a -> AppM a
runBeamTx pgM = do
    runDbTx $ \conn -> runBeam conn pgM

runBeam :: Pg.Connection -> Pg.Pg a -> AppM a
runBeam conn pgM = do
    cfg <- R.ask
    R.lift $ Pg.runBeamPostgresDebug (runAppM cfg . logDebug "SQL") conn pgM

runDbTx :: (Pg.Connection -> AppM a) -> AppM a
runDbTx action = do
    cfg <- R.ask
    withDbConn $ \conn ->
        R.lift $ PgSimple.withTransactionLevel PgSimple.Serializable conn
            (runAppM cfg $ action conn)

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

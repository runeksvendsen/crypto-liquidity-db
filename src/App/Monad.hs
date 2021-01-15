{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
module App.Monad
( module App.Monad
, R.lift
, R.ask
, Has(..)
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
import Data.Has ( Has(..) )
import qualified Control.Retry as Re
import Control.Monad.Catch ( Handler(Handler) )


type AppM r = R.ReaderT r IO

logInfo :: String -> String -> AppM r ()
logInfo ctx =
    R.lift . Log.logInfo (toS ctx)

logDebug :: String -> String -> AppM r ()
logDebug ctx =
    R.lift . logDebugIO ctx

logDebugIO :: String -> String -> IO ()
logDebugIO ctx = Log.logDebug (toS ctx)

logError :: String -> String -> AppM r ()
logError ctx =
    R.lift . Log.logError (toS ctx)

runAppM :: conf -> AppM conf a -> IO a
runAppM = flip R.runReaderT

withDbConn :: Has DbConn r => (Pg.Connection -> AppM r a) -> AppM r a
withDbConn f = do
    pool <- R.asks getter
    Pool.withResource pool f

runBeamTx :: Has DbConn r => Pg.Pg a -> AppM r a
runBeamTx pgM = do
    runDbTx $ \conn -> runBeam conn pgM

runBeam :: Pg.Connection -> Pg.Pg a -> AppM r a
runBeam conn pgM = do
    R.lift $ runBeamIO conn pgM

runBeamIO :: Pg.Connection -> Pg.Pg a -> IO a
runBeamIO conn pgM =
    Re.recovering policy [const pgSqlErrorHandler] (const $ runBeamIONoRetry conn pgM)
  where
    pgSqlErrorHandler = Handler $ \ex ->
        let (retry, msg) = shouldRetry ex
        in do
            when retry $ Log.logWarn "DATABASE" $ "Retrying error " ++ msg ++ ", message: " ++ show ex
            return retry
    shouldRetry err =
        let sqlState = Pg.sqlState err
            shouldRetry' = case sqlState of
                    "40001" -> True
                    _ -> False
        in (shouldRetry', toS sqlState)
    policy = Re.constantDelay 2000000 <> Re.limitRetries 10

runBeamIONoRetry :: Pg.Connection -> Pg.Pg a -> IO a
runBeamIONoRetry conn pgM = do
    Pg.runBeamPostgresDebug (logDebugIO "SQL") conn pgM

runDbTx :: Has DbConn r => (Pg.Connection -> AppM r a) -> AppM r a
runDbTx action = do
    cfg <- R.ask
    withDbConn $ \conn ->
        R.lift $ PgSimple.withTransactionLevel PgSimple.Serializable conn
            (runAppM cfg $ action conn)

async :: AppM r a -> AppM r (Async.Async a)
async appM = do
    cfg <- R.ask
    R.lift $ Async.async $ runAppM cfg appM

-- |
data Config = Config
    { cfgConstants :: CfgConstants
    , cfgDbConnPool :: Pool.Pool Pool.Connection
    } deriving (Generic, Has DbConn)

data CfgConstants = CfgConstants
    { cfgParams :: CfgParams
    , cfgMaxCalculationTime :: NominalDiffTime
      -- ^ the amount of time after which a calculation is considered stalled/dead
    , cfgDeadMonitorInterval :: NominalDiffTime
      -- ^ restart dead calculations this often
    }

data CfgParams = CfgParams
    { cfgNumeraires :: [Text]
    , cfgSlippages :: [Double]
    }

type DbConn = Pool.Pool Pool.Connection

calculationParameters :: CfgParams -> [(Text, Double)]
calculationParameters cfg = do
    numeraire <- cfgNumeraires cfg
    slippage <- cfgSlippages cfg
    return (numeraire, slippage)

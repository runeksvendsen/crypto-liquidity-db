{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module App.Monad
(
-- * Logging
  logInfo
, logDebug
, logDebugIO
, logError
-- * Monad
, AppM
, runAppM
, async
, Config(..)
, CfgConstants(..)
, CfgParams(..)
, DbConn
, calculationParameters
, runDbTx
, runDbTx_
, PgSimple.IsolationLevel(..)
, runDbTxWithConn
, runDbRaw
, runDbTxConn
, withDbConn
, DbTx
, asTx
, TxConn
, txConn
, R.lift
, R.ask
, Has(..)
 -- * Debugging
, dbTraceStatements
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
import qualified Data.ByteString.Char8 as BC


asTx :: Pg.Pg a -> DbTx a
asTx = DbTx

-- | A database connection that is inside a transaction
newtype TxConn = TxConn Pg.Connection
    deriving Eq

-- | Use this to run something that requires a 'Pg.Connection' as part of an existing transaction
txConn :: TxConn -> Pg.Connection
txConn (TxConn conn) = conn

runDbTxWithConnNoRetry :: Has DbConn r => PgSimple.IsolationLevel -> (TxConn -> DbTx a) -> AppM r a
runDbTxWithConnNoRetry level action = do
    withDbConn $ \conn ->
        R.lift $ PgSimple.withTransactionLevel level conn $ do
            let (DbTx pgM) = action (TxConn conn)
            runBeamIONoRetry conn pgM

newtype DbTx a = DbTx (Pg.Pg a)
    deriving (Functor, Applicative, Monad)

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

runDbTx :: Has DbConn r => DbTx a -> AppM r a
runDbTx dbTxM = runDbTxWithConn (const dbTxM)

runDbTx_ :: Has DbConn r => PgSimple.IsolationLevel -> DbTx a -> AppM r a
runDbTx_ level dbTxM = runDbTxWithConn_ level (const dbTxM)

-- | Run a database query outside of a transaction
runDbRaw :: Has DbConn r => Pg.Pg a -> AppM r a
runDbRaw pgM = do
    withDbConn $ \conn -> runDbTxConn (TxConn conn) pgM

-- | Run as part of an already-started transaction
runDbTxConn :: TxConn -> Pg.Pg a -> AppM r a
runDbTxConn (TxConn conn) pgM = do
    R.liftIO $ runBeamIONoRetry conn pgM

runDbTxWithConn :: Has DbConn r => (TxConn -> DbTx a) -> AppM r a
runDbTxWithConn = runDbTxWithConn_ PgSimple.Serializable

runDbTxWithConn_ :: Has DbConn r => PgSimple.IsolationLevel -> (TxConn -> DbTx a) -> AppM r a
runDbTxWithConn_ level action = do
    cfg <- R.ask
    R.liftIO $ Re.recovering policy [const pgSqlErrorHandler] (const $ runAppM cfg $ runDbTxWithConnNoRetry level action)
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

dbTraceStatements :: Has DbConn r => Pg.PgDebugStmt statement => statement -> AppM r BC.ByteString
dbTraceStatements statement =
    withDbConn $ \conn -> R.liftIO $ Pg.pgTraceStmtIO' conn statement

async :: AppM r a -> AppM r (Async.Async a)
async appM = do
    cfg <- R.ask
    R.lift $ Async.async $ runAppM cfg appM

-- |
data Config = Config
    { cfgConstants :: CfgConstants
    , cfgDbConnPool :: Pool.Pool Pool.Connection
    } deriving (Generic)

instance Has DbConn Config

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

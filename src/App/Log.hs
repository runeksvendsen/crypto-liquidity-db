module App.Log
( logInfo
, logDebug
, Log.withStdoutLogging
, Log.withStderrLogging
)
where

import qualified Control.Logging as Log
import qualified Data.Text as T


logInfo :: T.Text -> String -> IO ()
logInfo ctx msg = do
    Log.loggingLogger Log.LevelInfo ctx msg
    Log.flushLog

logDebug :: T.Text -> String -> IO ()
logDebug ctx msg = do
    Log.loggingLogger Log.LevelDebug ctx msg
    Log.flushLog

{-# LANGUAGE OverloadedStrings #-}
module App.Log
( logInfo
, logDebug
, logError
, logTrace
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

logTrace :: T.Text -> String -> IO ()
logTrace ctx msg = do
    Log.loggingLogger (Log.LevelOther "TRACE") ctx msg
    Log.flushLog

logError :: T.Text -> String -> IO ()
logError ctx msg = do
    Log.loggingLogger Log.LevelError  ctx msg
    Log.flushLog

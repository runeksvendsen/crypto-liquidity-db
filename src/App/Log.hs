{-# LANGUAGE OverloadedStrings #-}
module App.Log
( logInfo
, logDebug
, logWarn
, logError
, logTrace
, withLogging
, Log.withStdoutLogging
, Log.withStderrLogging
, Log.setLogTimeFormat
, Log.setLogLevel
, Log.LogLevel(..)
)
where

import qualified Control.Logging as Log
import qualified Data.Text as T
import Control.Concurrent (ThreadId, myThreadId)
import System.IO.Unsafe (unsafePerformIO)
import qualified Control.Logging
import Protolude.Conv (toS)
import qualified System.Console.ANSI as ANSI
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import Control.Monad.IO.Class (liftIO, MonadIO)

withLogging :: IO a -> IO a
withLogging io = do
    Log.withStdoutLogging $ do
      Log.setLogTimeFormat "%T.%3q"
      io

logDebug :: MonadIO m => T.Text -> String -> m ()
logDebug ctx msg = liftIO $
    logGenericIO Control.Logging.LevelDebug logFun ctx msg
  where
    logFun = Control.Logging.debug' . toS

logTrace :: MonadIO m => T.Text -> String -> m ()
logTrace ctx msg = liftIO $
    logGenericIO Control.Logging.LevelDebug logFun ctx msg
  where
    logFun = Log.loggingLogger (Log.LevelOther "TRACE") ctx

logInfo :: MonadIO m => T.Text -> String -> m ()
logInfo ctx msg = liftIO $
    logGenericIO Control.Logging.LevelInfo logFun ctx msg
  where
    logFun = Control.Logging.loggingLogger Control.Logging.LevelInfo ""

logWarn :: MonadIO m => T.Text -> String -> m ()
logWarn ctx msg = liftIO $
    logGenericIO Control.Logging.LevelWarn logFun ctx msg
  where
    logFun = Control.Logging.loggingLogger Control.Logging.LevelWarn ""

error' :: String -> IO ()
error' = Control.Logging.loggingLogger Control.Logging.LevelError ""

logError :: MonadIO m => T.Text -> String -> m ()
logError ctx msg = liftIO $
    logGenericIO Control.Logging.LevelError error' ctx msg

logGenericIO :: Control.Logging.LogLevel -> (String -> IO ()) -> T.Text -> String -> IO ()
logGenericIO lvl f ctx msg = do
    tid <- myThreadId
    f $ mkMsg lvl tid ctx msg

mkMsg :: Control.Logging.LogLevel -> ThreadId -> T.Text -> String -> String
mkMsg lvl tid ctx msg = unwords
    [ color ANSI.Magenta $ bracketize (toS ctx ++ "/" ++ showThreadId tid)
    , color (levelColor lvl) msg
    ]
  where
    bracketize str = "[" ++ str ++ "]"

showThreadId :: ThreadId -> String
showThreadId tid = fromMaybe "X" $ stripPrefix "ThreadId " $ show tid

levelColor :: Control.Logging.LogLevel -> ANSI.Color
levelColor lvl = case lvl of
    Control.Logging.LevelDebug -> ANSI.White
    Control.Logging.LevelInfo -> ANSI.Green
    Control.Logging.LevelWarn -> ANSI.Yellow
    Control.Logging.LevelError -> ANSI.Red
    Control.Logging.LevelOther _ -> ANSI.Black

color :: ANSI.Color -> String -> String
color color' str = concat
    [ ANSI.setSGRCode [ANSI.SetColor ANSI.Foreground ANSI.Dull color']
    , str
    , ANSI.setSGRCode [ANSI.Reset]
    ]

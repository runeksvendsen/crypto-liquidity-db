module App.Util
( currentTime
)
where

import Data.Time.Clock (getCurrentTime)
import Data.Time.LocalTime (LocalTime, utcToLocalTime)


currentTime :: IO LocalTime
currentTime = utcToLocalTime (read "UTC") <$> getCurrentTime

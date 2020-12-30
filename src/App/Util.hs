module App.Util
( currentTime
, addLocalTime
)
where

import Data.Time.Clock (getCurrentTime)
import Data.Time.LocalTime (LocalTime, addLocalTime, utcToLocalTime)


currentTime :: IO LocalTime
currentTime = utcToLocalTime (read "UTC") <$> getCurrentTime

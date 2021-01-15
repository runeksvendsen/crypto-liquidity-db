module App.Util
( currentTime
, addUTCTime
)
where

import Data.Time.Clock (getCurrentTime)
import Data.Time (UTCTime, addUTCTime)


currentTime :: IO UTCTime
currentTime = getCurrentTime

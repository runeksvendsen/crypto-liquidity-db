module App.Util
( currentTime
, addUTCTime
, prefixOptions
)
where

import Data.Time.Clock (getCurrentTime)
import Data.Time (UTCTime, addUTCTime)
-- Json
import qualified Data.Aeson as Json
import qualified Data.Aeson.Casing as Casing
import qualified  Data.Aeson.Casing.Internal as Casing

currentTime :: IO UTCTime
currentTime = getCurrentTime

prefixOptions :: Json.Options
prefixOptions = Json.defaultOptions
    { Json.fieldLabelModifier = dropFirstWord Casing.snakeCase
    , Json.sumEncoding = Json.ObjectWithSingleField
    , Json.constructorTagModifier = Casing.snakeCase
    }
  where
    -- Drop the first word (until first capital letter) and
    --  then apply a casing function
    dropFirstWord :: (String -> String) -> (String -> String)
    dropFirstWord f = f . Casing.dropFPrefix

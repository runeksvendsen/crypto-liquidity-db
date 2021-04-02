{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NumDecimals #-}

module App.Main.WebApi.Cache
( Public(..)
, toHeader
, TU.Second
, TU.Minute
, TU.Hour
, TU.Day
)
 where

import Internal.Prelude (toS)
import Servant hiding (toHeader)
import qualified Network.HTTP.Types.Header as HTTP
import qualified Data.Time.Units as TU


data Public
    = forall timeUnit. TU.TimeUnit timeUnit => MaxAge timeUnit

toHeader :: Public -> HTTP.Header
toHeader public = ("Cache-Control", toS $ toUrlPiece public)

instance ToHttpApiData Public where
    toUrlPiece (MaxAge timeUnit) =
        "public, max-age=" <> toS (show $ toSeconds timeUnit)

-- | Dummy implementation. For now we only care about the server, not the client.
instance FromHttpApiData Public where
    parseUrlPiece = const $ Right $ MaxAge (0 :: TU.Second)

toSeconds :: TU.TimeUnit a => a -> Integer
toSeconds = (`div` 1e6) . TU.toMicroseconds

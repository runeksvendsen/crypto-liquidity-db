{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Util
( isReady
, isReadyClientEnv
)
where

import qualified Servant.Client as SC
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Client (HttpException(..), HttpExceptionContent(..))
import Control.Exception (SomeException)
import qualified Control.Exception as Catch

isReadyClientEnv :: SC.ClientEnv -> IO Bool
isReadyClientEnv env =
    isReady man host port
  where
    man = SC.manager env
    host = SC.baseUrlHost $ SC.baseUrl env
    port = SC.baseUrlPort $ SC.baseUrl env

isReady :: HTTP.Manager -> String -> Int -> IO Bool
isReady man host port = do
    Catch.handle (pure . successfullyConnected) $ True <$ HTTP.httpNoBody request man
  where
    successfullyConnected = \case
        HttpExceptionRequest _ content -> case content of
            ConnectionTimeout -> False
            ConnectionFailure _ -> False
            StatusCodeException _ _ -> True
            other -> error $ "BUG: Unexpected failure in 'isReady': " <> show other
        InvalidUrlException url reason -> error $ "BUG: Invalid URL " <> url <> " reason: " <> reason

    request = either (\e -> error $ "BUG: Bad request: " <> show e) id requestE

    requestE :: Either SomeException HTTP.Request
    requestE = do
        let url = "http://" <> host <> ":" <> show port <> "/some-non-existing-path"
        initReq <- HTTP.parseRequest url
        pure $ initReq{ HTTP.method = "POST", HTTP.responseTimeout = HTTP.responseTimeoutMicro 1000000 }

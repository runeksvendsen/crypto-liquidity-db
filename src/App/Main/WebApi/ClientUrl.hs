module App.Main.WebApi.ClientUrl
( clientFUrl
)
where

import Data.Foldable (toList)
-- servant-client-core
import qualified Servant.Client.Free as SCF
import qualified Servant.Client.Core as SCC
-- free
import Control.Monad.Free ( Free(..) )
-- binary
import Data.Binary.Builder ( toLazyByteString )
-- bytestring
import qualified Data.ByteString.Lazy as BSL
-- http-types
import Network.HTTP.Types.URI ( renderQueryBuilder )


clientFUrl :: Free SCF.ClientF a -> BSL.ByteString
clientFUrl clientF = case clientF of
    Pure _ ->
        error "BUG: got 'Pure'"
    Free (SCF.Throw err) ->
        error $ "BUG: got 'Throw':" ++ show err
    Free (SCF.RunRequest req _) ->
        toLazyByteString $
            SCC.requestPath req <>
            renderQueryBuilder True (toList $ SCC.requestQueryString req)

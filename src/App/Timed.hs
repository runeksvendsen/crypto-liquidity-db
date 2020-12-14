module App.Timed where

import Protolude (NFData(..))
import GHC.Clock (getMonotonicTime)


timeEval
    :: (NFData a, NFData b)
    => (a -> IO b)
    -> a
    -> IO (b, Double)
timeEval f val = do
    _ <- return $ rnf val
    start <- getMonotonicTime
    res <- f val >>= \res -> return (rnf val) >> return res
    end <- getMonotonicTime
    return (res, end - start)

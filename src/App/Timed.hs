{-# LANGUAGE BangPatterns #-}
module App.Timed where

import Protolude (force, evaluate, NFData(..))
import GHC.Clock (getMonotonicTime)


timeEval
    :: (NFData a, NFData b)
    => (a -> IO b)
    -> a
    -> IO (b, Double)
timeEval f val = do
    _ <- evaluate (rnf val)
    start <- getMonotonicTime
    res <- f val >>= evaluate . force
    end <- getMonotonicTime
    let !delta = end - start
    return (res, delta)

timeEvalPure
    :: (NFData a, NFData b)
    => (a -> b)
    -> a
    -> IO (b, Double)
timeEvalPure f val = do
    _ <- evaluate (rnf val)
    start <- getMonotonicTime
    res <- evaluate $ force (f val)
    end <- getMonotonicTime
    let !delta = end - start
    return (res, delta)

-- Mutable priority queue
module App.MPQueue
( MQueue
, emptyIO
, insert
, removeMin
)
where

import qualified Control.Concurrent.STM.TVar as STM
import qualified Data.PQueue.Prio.Min as PQ
import qualified Control.Monad.STM as STM


-- |
newtype MQueue k a = MQueue (STM.TVar (PQ.MinPQueue k a))

-- |
insert :: Ord prio => prio -> a -> MQueue prio a -> STM.STM ()
insert k a (MQueue tVar) =
    STM.modifyTVar' tVar (PQ.insert k a)

-- |
emptyIO :: IO (MQueue k a)
emptyIO = MQueue <$> STM.newTVarIO PQ.empty

-- | NB: blocks until a value is available
removeMin :: Ord k => MQueue k a -> STM.STM a
removeMin (MQueue tVar) = do
   pQueue <- STM.readTVar tVar
   case PQ.minView pQueue of
       Nothing -> STM.retry
       Just (a, newPQueue) ->
           STM.writeTVar tVar newPQueue >> return a

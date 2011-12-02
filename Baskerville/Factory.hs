module Baskerville.Factory where

import Control.Concurrent

data Factory = Factory (MVar [ThreadId])

spawnFactory :: IO Factory
spawnFactory = do
    mv <- newMVar []
    return $ Factory mv

appendId :: ThreadId -> [ThreadId] -> IO ([ThreadId], ())
appendId t ts = return (t : ts, ())

removeId :: ThreadId -> [ThreadId] -> IO ([ThreadId], ())
removeId t ts = return (filter (/= t) ts, ())

spawnThread :: Factory -> IO () -> IO ThreadId
spawnThread (Factory mv) action = do
    tid <- forkIO action
    modifyMVar mv (appendId tid)
    return tid

removeThread :: Factory -> ThreadId -> IO ()
removeThread (Factory mv) tid = modifyMVar mv (removeId tid)

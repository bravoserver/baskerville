module Main where

import Control.Concurrent
import Control.Exception hiding (catch)
import Control.Monad
import Network
import System.IO
import System.IO.Error

-- | Guard an opened socket so that it will always close during cleanup.
--   This can and should be used in place of listenOn.
withListenOn port = bracket (listenOn port) sClose 
  
echo (handle, host, port) = catch (forever doOneLine) stop 
    where doOneLine = hGetLine handle >>= hPutStrLn handle >> hFlush handle
          stop error = do
            putStrLn $ "Closed connection from " ++ show (host, port) ++ " due to " ++ show error
            hClose handle

main = withSocketsDo $
    withListenOn (PortNumber 12321) $ \listener ->
        forever $ accept listener >>= forkIO . echo

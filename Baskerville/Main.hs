module Main where

import Control.Concurrent
import Control.Exception hiding (catch)
import Control.Monad
import Network hiding (accept)
import Network.Socket (accept)
import Data.IterIO

import Baskerville.Beta.Protocol

fork :: Socket -> IO ()
fork listener = forever $ do
    (sock, addr) <- accept listener
    putStrLn $ "Accepting connection from " ++ show addr ++ "..."
    pair <- iterStream sock
    _ <- forkIO (socketHandler pair)
    return ()

-- | Guard an opened socket so that it will always close during cleanup.
--   This can and should be used in place of listenOn.
withListenOn :: PortID -> (Socket -> IO a) -> IO a
withListenOn port = bracket (listenOn port) sClose 

startServer :: IO ()
startServer = withListenOn (PortNumber 12321) fork

main :: IO ()
main = withSocketsDo startServer

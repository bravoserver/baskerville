module Main where

import qualified Data.ByteString as BS
import Control.Concurrent
import Control.Exception hiding (catch)
import Control.Monad
import Network hiding (accept)
import Network.Socket (accept)
import Data.IterIO

import Baskerville.Beta.Packets

handler :: (Iter BS.ByteString IO (), Onum BS.ByteString IO ()) -> IO ()
handler (input, output) = output |$ input

fork :: Socket -> IO ()
fork listener = forever $ do
    (sock, addr) <- accept listener
    pair <- iterStream sock
    _ <- forkIO (handler pair)
    return ()

-- | Guard an opened socket so that it will always close during cleanup.
--   This can and should be used in place of listenOn.
withListenOn :: PortID -> (Socket -> IO a) -> IO a
withListenOn port = bracket (listenOn port) sClose 

startServer :: IO ()
startServer = withListenOn (PortNumber 12321) fork

main :: IO ()
main = withSocketsDo startServer

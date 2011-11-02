module Main where

import qualified Data.ByteString as BS
import Control.Concurrent
import Control.Exception hiding (catch)
import Control.Monad
import Network
import System.IO

chunk :: Handle -> IO ()
chunk h = BS.hGetLine h >>= BS.hPutStrLn h

stop :: Handle -> IOError -> IO ()
stop h e = hClose h

handler :: (Handle, HostName, PortNumber) -> IO ()
handler (h, _, _) = catch (forever (chunk h >> hFlush h)) (stop h)

-- | Guard an opened socket so that it will always close during cleanup.
--   This can and should be used in place of listenOn.
withListenOn :: PortID -> (Socket -> IO a) -> IO a
withListenOn port = bracket (listenOn port) sClose 

fork :: Socket -> IO ()
fork sock = forever $ accept sock >>= forkIO . handler

startServer :: IO ()
startServer = withListenOn (PortNumber 12321) fork

main :: IO ()
main = withSocketsDo startServer

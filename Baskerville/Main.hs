module Main where

import qualified Data.ByteString as BS
import Control.Concurrent
import Control.Exception hiding (catch)
import Control.Monad
import Network
import System.IO
import Data.Word

import Baskerville.Beta.Packets

char2word8 :: Char -> Word8
char2word8 = toEnum . fromEnum

str2bs :: [Char] -> BS.ByteString
str2bs s = BS.pack (map char2word8 s)

echo :: BS.ByteString -> BS.ByteString
echo bs = BS.intercalate (BS.pack [0x0d, 0x0a]) (map (str2bs . show) (getPackets bs))

-- | Perform incremental socket chunk handling.
--   This function reads chunks of up to 4096 bytes at a time from a socket,
--   and runs it through a pure function using ByteStrings.
chunk :: (BS.ByteString -> BS.ByteString) -> Handle -> IO ()
chunk f h = do
    bs <- BS.hGetSome h 4096
    BS.hPutStr h (f bs)

stop :: Handle -> IOError -> IO ()
stop h e = hClose h

handler :: (BS.ByteString -> BS.ByteString) -> (Handle, HostName, PortNumber) -> IO ()
handler f (h, _, _) = catch (forever (chunk f h >> hFlush h)) (stop h)

-- | Guard an opened socket so that it will always close during cleanup.
--   This can and should be used in place of listenOn.
withListenOn :: PortID -> (Socket -> IO a) -> IO a
withListenOn port = bracket (listenOn port) sClose 

fork :: Socket -> IO ()
fork sock = forever $ accept sock >>= forkIO . handler echo

startServer :: IO ()
startServer = withListenOn (PortNumber 12321) fork

main :: IO ()
main = withSocketsDo startServer

module Main where

import qualified Data.ByteString as BS
import Control.Concurrent
import Control.Exception hiding (catch)
import Control.Monad
import Network hiding (accept)
import Network.Socket (accept)
import Data.IterIO
import Data.IterIO.Atto
import Data.Word

import Baskerville.Beta.Packets

parser :: Iter BS.ByteString IO Packet
parser = atto parsePacket

char2word8 :: Char -> Word8
char2word8 = toEnum . fromEnum

str2bs :: [Char] -> BS.ByteString
str2bs s = BS.pack (map char2word8 s)

handler :: (Iter BS.ByteString IO (), Onum BS.ByteString IO Packet) -> IO ()
handler (output, input) = do
    packet <- input |$ parser
    inumPure (str2bs $ show packet) |$ output
    return ()

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

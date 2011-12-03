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
import Baskerville.Beta.Protocol

parser :: Monad m => Iter BS.ByteString m [Packet]
parser = atto parsePackets

char2word8 :: Char -> Word8
char2word8 = toEnum . fromEnum

str2bs :: String -> BS.ByteString
str2bs s = BS.pack (map char2word8 s)

handler :: Socket -> (Iter BS.ByteString IO (), Onum BS.ByteString IO [Packet]) -> IO ()
handler sock (output, input) = do
    putStrLn "Starting handler..."
    packetsIn <- input |$ parser
    putStrLn "Wired up input..."
    let packetsOut = processPacketStream packetsIn
    putStrLn "Processed packets..."
    inumPure (BS.concat $ map buildPacket packetsOut) |$ output
    putStrLn "Output data..."
    sClose sock
    putStrLn "Closed socket!"
    return ()

fork :: Socket -> IO ()
fork listener = forever $ do
    (sock, addr) <- accept listener
    putStrLn $ "Accepting connection from " ++ show addr ++ "..."
    pair <- iterStream sock
    _ <- forkIO (handler sock pair)
    return ()

-- | Guard an opened socket so that it will always close during cleanup.
--   This can and should be used in place of listenOn.
withListenOn :: PortID -> (Socket -> IO a) -> IO a
withListenOn port = bracket (listenOn port) sClose 

startServer :: IO ()
startServer = withListenOn (PortNumber 12321) fork

main :: IO ()
main = withSocketsDo startServer

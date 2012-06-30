module Main where

import Network
import qualified Data.ByteString as BS
import Data.Conduit
import Data.Conduit.Cereal
import Data.Conduit.Network
import Data.Serialize

import Baskerville.Beta.Packets
import Baskerville.Beta.Protocol

toPackets :: Conduit BS.ByteString IO Packet
toPackets = conduitGet $ get

fromPackets :: Conduit Packet IO BS.ByteString
fromPackets = conduitPut $ put

app :: Application IO
app source sink = do
    putStrLn "Before app..."
    let pSource = source $= toPackets
    let pSink = fromPackets =$ sink
    pSource $$ protocol =$ pSink
    putStrLn "After app!"

startServer :: IO ()
startServer = runTCPServer (ServerSettings 12321 HostAny) app

main :: IO ()
main = withSocketsDo $ do
    putStrLn "Starting up..."
    startServer

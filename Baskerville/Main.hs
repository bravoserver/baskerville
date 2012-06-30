module Main where

import Network
import qualified Data.Bytestring as BS
import Data.Conduit
import Data.Conduit.Cereal
import qualified Data.Conduit.List as CL
import Data.Conduit.Network
import Data.Serialize

import Baskerville.Beta.Packets

toPackets :: Conduit BS.ByteString IO Packet
toPackets = conduitGet $ get

fromPackets :: Conduit Packet IO BS.ByteString
fromPackets = conduitPut $ put

worker :: Conduit Packet IO Packet
worker = CL.mapM $ \packet -> do
    putStrLn "Got a packet!"
    putStrLn $ show packet
    return packet

app :: Application IO
app source sink = do
    putStrLn "Before app..."
    let pSource = source $= toPackets
    let pSink = fromPackets =$ sink
    pSource $$ worker =$ pSink
    putStrLn "After app!"

startServer :: IO ()
startServer = runTCPServer (ServerSettings 12321 HostAny) app

main :: IO ()
main = withSocketsDo $ do
    putStrLn "Starting up..."
    startServer

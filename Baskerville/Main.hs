module Main where

import Control.Concurrent hiding (yield)
import Control.Concurrent.STM
import Control.Monad.IO.Class
import Data.Conduit
import Data.Conduit.Network
import Network

import Baskerville.Beta.Conduits
import Baskerville.Beta.Packets
import Baskerville.Beta.Session

intake :: TChan (Maybe Packet) -> Sink Packet IO ()
intake chan = loop
    where
    loop = do
        mpacket <- await
        case mpacket of
            Nothing -> liftIO $ putStrLn "Finished reading!"
            Just _  -> do
                liftIO . atomically $ writeTChan chan mpacket
                loop

outflow :: TChan (Maybe Packet) -> Source IO Packet
outflow chan = loop
    where
    loop = do
        mpacket <- liftIO . atomically $ readTChan chan
        case mpacket of
            Nothing     -> liftIO $ putStrLn "Finished writing!"
            Just packet -> do
                yield packet
                loop

makeChans :: () -> STM (TChan (Maybe Packet), TChan (Maybe Packet))
makeChans core = do
    incoming <- newTChan
    outgoing <- newTChan
    return (incoming, outgoing)

app :: TVar () -> Application IO
app tcore appdata = do
    putStrLn "Before app..."
    let pSource = appSource appdata $= bsToPackets
        pSink = packetsToBs =$ appSink appdata
    (incoming, outgoing) <- atomically $ makeChans ()
    liftIO . forkIO $ pSource $$ intake incoming
    liftIO . forkIO $ outflow outgoing $$ pSink
    packetThread incoming outgoing
    putStrLn "After app!"

startServer :: TVar () -> IO ()
startServer tcore = runTCPServer (serverSettings 25565 HostAny) $ app tcore

main :: IO ()
main = withSocketsDo $ do
    putStrLn "Starting up..."
    core <- atomically $ newTVar ()
    startServer core

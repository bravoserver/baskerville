{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent hiding (yield)
import Control.Concurrent.STM
import Control.Monad.IO.Class
import Data.Conduit
import Data.Conduit.Cereal
import qualified Data.Conduit.List as CL
import Data.Conduit.Network
import Data.Serialize
import Network

import Baskerville.Beta.Conduits
import Baskerville.Beta.Login
import Baskerville.Beta.Packets
import Baskerville.Beta.Session
import Baskerville.Beta.Shake

intake :: TChan (Maybe Packet) -> Sink Packet IO ()
intake chan = awaitForever worker
    where
    worker packet = liftIO . atomically $ writeTChan chan (Just packet)

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

statusConduit :: Conduit StatusPacket IO StatusPacket
statusConduit = awaitForever worker
    where
    worker packet = yield $ case packet of
        StatusRequest -> StatusResponse
        _             -> packet

loginConduit :: Conduit LoginPacket IO LoginPacket
loginConduit = awaitForever worker
    where
    worker packet = do
        liftIO . print $ packet
        yield $ case packet of
            (LoginStart username) -> LoginSuccess "" username
            _                     -> packet

app :: TVar () -> Application IO
app tcore appdata = do
    putStrLn "Before app..."
    let handshakeSink = conduitGet getHandshake =$ CL.head
    (rsource, handshake) <- appSource appdata $$+ handshakeSink
    case handshake of
        Nothing -> return ()
        Just (Handshake _ _ _ style) -> do
            print handshake
            (source, finalizer) <- unwrapResumable rsource
            case style of
                NewStatus -> do
                    let source' = source $= conduitGet getStatus
                        dest    = conduitPut putStatus =$ appSink appdata
                    source' $= statusConduit $$ dest
                NewLogin -> do
                    let source' = source $= conduitGet getLogin
                        dest    = conduitPut putLogin =$ appSink appdata
                    source' $= loginConduit $$ dest
                    -- (incoming, outgoing) <- atomically $ makeChans ()
                    -- forkIO $ source' $$ intake incoming
                    -- forkIO $ outflow outgoing $$ dest
                    -- packetThread incoming outgoing
            finalizer
    putStrLn "After app!"

startServer :: TVar () -> IO ()
startServer tcore = runTCPServer (serverSettings 25565 HostAny) $ app tcore

main :: IO ()
main = withSocketsDo $ do
    putStrLn "Starting up..."
    core <- atomically $ newTVar ()
    startServer core

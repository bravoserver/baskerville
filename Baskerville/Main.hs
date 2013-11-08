{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent hiding (yield)
import Control.Concurrent.STM
import Control.Lens
import Control.Monad.IO.Class
import qualified Data.ByteString as BS
import Data.Conduit
import Data.Conduit.Cereal
import qualified Data.Conduit.List as CL
import Data.Conduit.Network
import Data.Serialize
import Network

import Baskerville.Beta.Login
import Baskerville.Beta.Packets
import Baskerville.Beta.Server
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

makeChans :: Server -> STM (TChan (Maybe Packet), TChan (Maybe Packet))
makeChans core = do
    incoming <- newTChan
    outgoing <- newTChan
    return (incoming, outgoing)

statusConduit :: Server -> Conduit StatusPacket IO StatusPacket
statusConduit server = awaitForever worker
    where
    worker packet = yield $ case packet of
        StatusRequest -> StatusResponse $ server ^. sInfo
        _             -> packet

loginPacket :: LoginPacket -> LoginPacket
loginPacket (LoginStart username) = LoginSuccess "" username
loginPacket packet = packet

justOne :: Get a -> Source IO BS.ByteString
        -> IO (ResumableSource IO BS.ByteString, Maybe a)
justOne getter source = source $$+ conduitGet getter =$ CL.head

app :: TVar Server -> Application IO
app tcore appdata = do
    putStrLn "Handling client connection..."
    let outSink = appSink appdata
    (rsource, handshake) <- justOne getHandshake $ appSource appdata
    case handshake of
        Nothing -> return ()
        Just (Handshake _ _ _ style) -> do
            print handshake
            (source, finalizer) <- unwrapResumable rsource
            server <- readTVarIO tcore
            case style of
                NewStatus -> do
                    let source' = source $= conduitGet getStatus
                        dest    = conduitPut putStatus =$ appSink appdata
                    source' $= statusConduit server $$ dest
                NewLogin -> do
                    let source' = source $= conduitGet getLogin
                        dest    = conduitPut putLogin =$ outSink
                    -- Note that the Sink is impure and can be reused. Yay?
                    (rsource', login) <- justOne getLogin $ source
                    case login of
                        Nothing -> return ()
                        Just login' -> do
                            -- Send a single Login packet.
                            CL.sourceList [loginPacket login'] $$ conduitPut putLogin =$ outSink
                            -- (incoming, outgoing) <- atomically $ makeChans server
                            -- forkIO $ rsource' $$+- intake incoming
                            -- forkIO $ outflow outgoing $$ dest
                            -- packetThread incoming outgoing
            finalizer
    putStrLn "Finished handling client!"

startServer :: TVar Server -> IO ()
startServer tcore = runTCPServer (serverSettings 25565 HostAny) $ app tcore

main :: IO ()
main = withSocketsDo $ do
    putStrLn "Starting up..."
    core <- atomically . newTVar $ Server (Info Version)
    startServer core

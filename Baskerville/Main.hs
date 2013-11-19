{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent hiding (yield)
import Control.Concurrent.STM
import Control.Lens
import Control.Monad
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
import Baskerville.Beta.Session
import Baskerville.Beta.Shake
import Baskerville.Utilities.Control

intake :: TChan (Maybe IncomingPacket) -> Sink IncomingPacket IO ()
intake chan = awaitForever worker
    where
    worker packet = liftIO . atomically $ writeTChan chan (Just packet)

outflow :: TChan (Maybe OutgoingPacket) -> Source IO OutgoingPacket
outflow chan = loop
    where
    loop = do
        mpacket <- liftIO . atomically $ readTChan chan
        whenJust mpacket $ \packet -> do
            yield packet
            loop

makeChans :: Server
          -> STM (TChan (Maybe IncomingPacket), TChan (Maybe OutgoingPacket))
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
loginPacket (LoginStart username) = LoginSuccess "0123456789abcdef" username
loginPacket packet = packet

justOne :: Get a -> Source IO BS.ByteString
        -> IO (ResumableSource IO BS.ByteString, Maybe a)
justOne getter source = source $$+ conduitGet getter =$ CL.head

app :: TVar Server -> Application IO
app tcore appdata = do
    putStrLn "Handling client connection..."
    let outSink = appSink appdata
    (rsource, handshake) <- justOne getHandshake $ appSource appdata
    whenJust handshake $ \(Handshake _ _ _ style) -> do
        print handshake
        (source, finalizer) <- unwrapResumable rsource
        server <- readTVarIO tcore
        case style of
            NewStatus -> do
                let source' = source $= conduitGet getStatus
                    dest    = conduitPut putStatus =$ appSink appdata
                source' $= statusConduit server $$ dest
            NewLogin -> do
                (rsource', login) <- justOne getLogin source
                whenJust login $ \login' -> do
                    -- Send a single Login packet.
                    CL.sourceList [loginPacket login'] $$ conduitPut putLogin =$ outSink
                    (incoming, outgoing) <- atomically $ makeChans server
                    void . forkIO $ rsource' $$+- conduitGet getPacket =$ intake incoming
                    -- Note that the Sink is impure and can be reused. Yay?
                    void . forkIO $ outflow outgoing $= conduitPut putPacket $$ outSink
                    packetThread incoming outgoing
        finalizer
    putStrLn "Finished handling client!"

startServer :: TVar Server -> IO ()
startServer tcore = runTCPServer (serverSettings 25565 HostAny) $ app tcore

main :: IO ()
main = withSocketsDo $ do
    putStrLn "Starting up..."
    core <- atomically . newTVar $ Server (Info Version)
    startServer core

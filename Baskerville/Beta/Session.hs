{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Baskerville.Beta.Session where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.RWS
import qualified Data.Map as M
import Data.Time
import qualified Data.Text as T
import Data.Word

import Baskerville.Beta.Packets
import Baskerville.Chunk
import Baskerville.Chunk.Generators

showText :: Show a => a -> T.Text
showText = T.pack . show

data Session = Session { _ssValid :: Bool
                       , _ssPings :: M.Map Word32 UTCTime
                       , _ssCurrentPing :: Word32
                       , _ssNick :: T.Text
                       }
    deriving (Show)

makeLenses ''Session

-- | The default starting state for a protocol.
startingState :: Session
startingState = Session True M.empty 1 T.empty

type Worker = RWST () [OutgoingPacket] Session IO

sendp :: TChan (Maybe OutgoingPacket) -> OutgoingPacket -> STM ()
sendp chan p = writeTChan chan $ Just p

pingThread :: TChan (Maybe OutgoingPacket) -> TMVar Session -> IO ()
pingThread chan tmc = loop
    where
    loop = do
        -- The client appears to often die if pings are sent *immediately*
        -- after login, so we'll send them *after* our timed delay.
        threadDelay $ 10 * 1000 * 1000
        client <- atomically $ takeTMVar tmc
        (client', pid) <- makePing client
        atomically $ do
            putTMVar tmc client'
            sendp chan $ Ping pid
        loop

packetThread :: TChan (Maybe IncomingPacket)
             -> TChan (Maybe OutgoingPacket)
             -> IO ()
packetThread incoming outgoing = do
    client <- atomically $ do
        greet
        newTMVar startingState
    pingThreadId <- forkIO $ pingThread outgoing client
    atomically $ do
        forM_ [-16..16] $ \x -> forM_ [-16..16] $ \z ->
            sendp outgoing $ ChunkData (boringChunk (ChunkIx (x, z)))
        sendp outgoing $ ServerLocation 0 130 0 0 0 Aloft
    loop client
    killThread pingThreadId
    where
    boringChunk i = runGenerator boring $ newChunk i
    greet = sendp outgoing $ Join (EID 42) Creative Earth Peaceful 42 "default"
    end = writeTChan outgoing Nothing
    loop tmc = do
        mp <- atomically $ readTChan incoming
        case mp of
            Nothing -> atomically end
            Just packet -> do
                c <- atomically $ takeTMVar tmc
                ((), c', w) <- runRWST (process packet) () c
                atomically $ putTMVar tmc c'
                atomically $ forM_ w $ sendp outgoing
                loop tmc

invalidate :: Worker ()
invalidate = ssValid .= False

-- | Kick the client and invalidate the session.
kick :: T.Text -> Worker ()
kick s = tell [Error s] >> invalidate

-- | Broadcast to everybody.
-- broadcast :: (MonadIO m) => Packet -> Conduit Packet (Session m) Packet
-- broadcast packet = do
--     chan <- lift $ use ssBroadcast
--     liftIO . atomically $ writeTChan chan packet

-- | Make a new ping.
makePing :: Session -> IO (Session, Word32)
makePing s = let key = s ^. ssCurrentPing in do
    time <- getCurrentTime
    let s' = s & ssPings . at key ?~ time & ssCurrentPing +~ 1
    return (s', key)

-- | Receive a ping.
handlePing :: Word32 -> Worker ()
handlePing key = do
    m <- use ssPings
    case M.lookup key m of
        -- The client will often send pongs on its own, unsolicited. In those
        -- cases, just ignore them; they are acting as keepalives and have no
        -- useful latency information.
        Nothing    -> return ()
        Just start -> do
            liftIO $ do
                end <- getCurrentTime
                putStrLn $ "Ping: " ++ show (diffUTCTime end start)
            ssPings . at key .= Nothing

-- | The main entry point for a worker handling a client.
--   Run this function over a packet and receive zero or more packets in
--   reply, as well as updates to the client state.
process :: IncomingPacket -> Worker ()

-- | A ping or keep alive packet. Send one back after receiving one from the
--   client. We should actually just take this opportunity to update the
--   latency numbers...
process (Pong pid) = handlePing pid

-- | Chat packet. Broadcast it to everybody else.
-- process cp@(Chat _) = broadcast cp

process (ClientAirborne{}) = return ()
process (ClientPosition{}) = return ()
process (ClientLocation{}) = return ()
process (ClientAnimation{}) = return ()
-- process (SlotSelection _) = return ()
process (ClientSettings{}) = return ()

-- | Plugin messages.
process (PluginMessage channel bytes) =
    case channel of
        "MC|Brand" -> lift . putStrLn $ "Client branding: " ++ show bytes
        _          -> kick $ T.concat ["Unknown channel ", channel]

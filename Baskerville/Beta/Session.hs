{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Baskerville.Beta.Session where

import Control.Concurrent.STM
import Control.Lens
import Control.Monad
import Control.Monad.Trans.RWS
import qualified Data.Text as T

import Baskerville.Beta.Packets

data Session = Session { _ssValid :: Bool
                       , _ssNick :: T.Text
                       }
    deriving (Show)

makeLenses ''Session

-- | The default starting state for a protocol.
startingState :: Session
startingState = Session True T.empty

type Worker = RWST () [Packet] Session IO

packetThread :: TChan (Maybe Packet) -> TChan (Maybe Packet) -> IO ()
packetThread incoming outgoing = loop startingState
    where
    loop s = do
        putStrLn "Start"
        mp <- atomically $ readTChan incoming
        case mp of
            Nothing -> return ()
            Just packet -> do
                putStrLn $ "Got a " ++ show packet ++ " packet!"
                ((), s', w) <- runRWST (process packet) () s
                atomically $ forM_ w $ \p -> writeTChan outgoing (Just p)
                putStrLn "End"
                loop s'

invalidate :: Worker ()
invalidate = ssValid .= False

kick :: T.Text -> Worker ()
kick s = do
    tell [ErrorPacket s]
    invalidate

-- | Broadcast to everybody.
-- broadcast :: (MonadIO m) => Packet -> Conduit Packet (Session m) Packet
-- broadcast packet = do
--     chan <- lift $ use ssBroadcast
--     liftIO . atomically $ writeTChan chan packet

-- | The main entry point for a protocol.
--   Run this function over a packet and receive zero or more packets in
--   reply. This function should be provided with state so that it can
--   process consecutive packets.
--   The type requires a Monad constraint in order to function correctly with
--   StateT, but doesn't require IO in order to faciliate possible refactoring
--   down the road.
process :: Packet -> Worker ()

-- | A ping or keep alive packet. Send one back after receiving one from the
--   client.
process (PingPacket _) = tell [PingPacket 0]

-- | Handshake. Reply with a login.
process (HandshakePacket nick _ _) = do
    ssNick .= nick
    tell [LoginPacket (EID 1) "default" Creative Earth Peaceful 10]

-- | Chat packet. Broadcast it to everybody else.
-- process cp@(ChatPacket _) = broadcast cp

-- | A poll. Reply with a formatted error packet and close the connection.
process PollPacket = kick "Baskerville§0§1"

-- | An error on the client side. They have no right to do this, but let them
--   get away with it anyway. They clearly want to be disconnected, so
--   disconnect them.
process (ErrorPacket _) = invalidate

-- | A packet which we don't handle. Kick the client, we're wasting time here.
process _ = kick "I refuse to handle this packet."

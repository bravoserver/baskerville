{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Baskerville.Beta.Session where

import Control.Concurrent.STM
import Control.Lens
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.RWS
import qualified Data.Text as T

import Baskerville.Beta.Packets

showText :: Show a => a -> T.Text
showText = T.pack . show

data Session = Session { _ssValid :: Bool
                       , _ssNick :: T.Text
                       }
    deriving (Show)

makeLenses ''Session

-- | The default starting state for a protocol.
startingState :: Session
startingState = Session True T.empty

type Worker = RWST () [OutgoingPacket] Session IO

packetThread :: TChan (Maybe IncomingPacket)
             -> TChan (Maybe OutgoingPacket)
             -> IO ()
packetThread incoming outgoing = greet >> loop startingState
    where
    greet = atomically $ writeTChan outgoing (Just (Join (EID 42) Creative Earth Peaceful 42 "default"))
    end = writeTChan outgoing Nothing
    loop s = do
        putStrLn "Start"
        mp <- atomically $ readTChan incoming
        case mp of
            Nothing -> atomically end
            Just packet -> do
                putStrLn $ "Got a " ++ show packet ++ " packet!"
                ((), s', w) <- runRWST (process packet) () s
                print w
                atomically $ forM_ w $ \p -> writeTChan outgoing (Just p)
                putStrLn "End"
                loop s'

invalidate :: Worker ()
invalidate = ssValid .= False

kick :: T.Text -> Worker ()
kick s = do
    -- tell [Error s]
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
process :: IncomingPacket -> Worker ()

-- | A ping or keep alive packet. Send one back after receiving one from the
--   client. We should actually just take this opportunity to update the
--   latency numbers...
process (Pong _) = return () -- tell [Ping p]

-- | Handshake. Reply with a login.
-- process (Handshake protocol nick _ _) =
--     if protocol /= 78
--         then kick $ T.append "Bad protocol " (showText protocol)
--         else do
--             ssNick .= nick
--             lift . putStrLn $ "Shook hands with " ++ T.unpack nick
--             tell [Login (EID 1) "default" Creative Earth Peaceful 10]

-- | Chat packet. Broadcast it to everybody else.
-- process cp@(Chat _) = broadcast cp

-- process (AirbornePacket _) = return ()
process (ClientPosition{}) = return ()
process (ClientLocation{}) = return ()
-- process (SlotSelection _) = return ()
process (ClientSettings{}) = return ()

-- | Plugin messages.
process (PluginMessage channel bytes) =
    case channel of
        "MC|Brand" -> lift . putStrLn $ "Client branding: " ++ show bytes
        _          -> return ()

-- | An error on the client side. They have no right to do this, but let them
--   get away with it anyway. They clearly want to be disconnected, so
--   disconnect them.
-- process (Error _) = invalidate

-- | A packet which we don't handle. Kick the client, we're wasting time here.
process _ = kick "I refuse to handle this packet."

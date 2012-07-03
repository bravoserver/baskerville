{-# LANGUAGE TemplateHaskell #-}

module Baskerville.Beta.Session where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.Conduit
import Data.Lens.Lazy
import Data.Lens.Template
import qualified Data.Text as T

import Baskerville.Beta.Packets

data SessionStatus = Invalid | Connected | Authenticated | Located
   deriving (Eq, Show)

data SessionState = SessionState { _ssStatus :: SessionStatus
                                 , _ssNick :: T.Text
                                 }
    deriving (Show)

$( makeLens ''SessionState )

type Session m = StateT SessionState m

-- | The default starting state for a protocol.
startingState :: SessionState
startingState = SessionState Connected T.empty

-- | Repeatedly read in packets, process them, and output them.
--   Internally holds the state required for a protocol.
worker :: Conduit Packet (Session IO) Packet
worker = do
    mpacket <- await
    case mpacket of
        Nothing -> liftIO $ putStrLn "No more packets!"
        Just InvalidPacket -> liftIO $ putStrLn "Invalid packet!"
        Just packet -> do
            liftIO $ putStrLn "Got a packet!"
            liftIO . putStrLn $ show packet
            processPacket packet
            status <- lift $ access ssStatus
            unless (status == Invalid) worker

protocol :: Conduit Packet IO Packet
protocol = let
    runner :: Session IO a -> IO a
    runner = flip evalStateT startingState
    in transPipe runner worker

invalidate :: (Monad m) => Conduit Packet (Session m) Packet
invalidate = lift $ ssStatus ~= Invalid >> return ()

errorOut :: (Monad m) => String -> Conduit Packet (Session m) Packet
errorOut s = invalidate >> (yield $ ErrorPacket $ T.pack s)

-- | The main entry point for a protocol.
--   Run this function over a packet and receive zero or more packets in
--   reply. This function should be provided with state so that it can
--   process consecutive packets.
--   The type requires a Monad constraint in order to function correctly with
--   StateT, but doesn't require IO in order to faciliate possible refactoring
--   down the road.
processPacket :: (Monad m) => Packet -> Conduit Packet (Session m) Packet

-- | Login. Examine all of the bits, make sure they match, and then reply in
--   kind.
processPacket (LoginPacket protoVersion _ _ _ _ _ _ _) =
    -- Is the protocol invalid? Kick the client with an unsupported-protocol
    -- message.
    if protoVersion /= 22
        then do
            errorOut "Unsupported protocol"
        else do
            _ <- lift $ ssStatus ~= Authenticated
            yield $ LoginPacket 1 T.empty 0 Creative Earth Peaceful 128 10

-- | Handshake. Just write down the username.
processPacket (HandshakePacket nick) = do
    _ <- lift $ ssNick ~= nick
    yield $ HandshakePacket $ T.pack "-"

-- | A poll. Reply with a formatted error packet and close the connection.
processPacket PollPacket = do
    errorOut "Baskerville§0§1"

-- | An error on the client side. They have no right to do this, but let them
--   get away with it anyway. They clearly want to be disconnected, so
--   disconnect them.
processPacket (ErrorPacket _) = invalidate

-- | A packet which we don't handle. Kick the client, we're wasting time here.
processPacket _ = invalidate

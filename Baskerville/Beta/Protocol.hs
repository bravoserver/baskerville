module Baskerville.Beta.Protocol where

import Control.Monad
import Control.Monad.ST
import Control.Monad.Trans.Class
import qualified Data.ByteString as BS
import Data.List
import Data.Serialize
import Data.STRef
import qualified Data.Text as T

import Baskerville.Beta.Packets

data ProtocolStatus = Invalid | Connected | Authenticated | Located
   deriving (Eq, Show)

data ProtocolState = ProtocolState { psStatus :: ProtocolStatus
                                   , psNick :: T.Text
                                   }
    deriving (Show)

-- | The default starting state for a protocol.
startingState :: ProtocolState
startingState = ProtocolState Connected T.empty

-- | Repeatedly read in packets, process them, and output them.
--   Internally holds the state required for a protocol.
-- pipeline :: Inum BS.ByteString BS.ByteString IO a
-- pipeline = mkInumAutoM $ loop startingState
--     where loop ps = do
--             lift $ lift $ putStrLn $ "Top of the pipeline, state " ++ show ps
--             packet <- atto parsePacket
--             lift $ lift $ putStrLn $ "Parsed a packet: " ++ show packet
--             let (state, packets) = processPacket ps packet
--             lift $ lift $ putStrLn $ "Processed a packet, state " ++ show state
--             _ <- ifeed $ BS.concat $ map encode $ takeWhile invalidPred packets
--             lift $ lift $ putStrLn "Fed the iteratee!"
--             when (psStatus state == Invalid) idone
--             lift $ lift $ putStrLn "Getting ready to loop!"
--             loop state
-- 
-- socketHandler :: (Iter BS.ByteString IO a, Onum BS.ByteString IO a) -> IO ()
-- socketHandler (output, input) = do
--     putStrLn "Starting pipeline..."
--     _ <- input |$ pipeline .| output
--     putStrLn "Finished pipeline!"

-- | A helper for iterating over an infinite packet stream and returning
--   another infinite packet stream in return. When in doubt, use this.
processPacketStream :: [Packet] -> [Packet]
processPacketStream packets =
    let mapper = concat . snd . mapAccumL processPacket startingState
    in takeWhile invalidPred $ mapper packets

-- | Determine whether a packet is an InvalidPacket.
--   This is used as a predicate for determining when to finish the packet
--   stream; InvalidPacket is always the end of the line. Note that the values
--   are inverted since this will be passed to takeWhile.
invalidPred :: Packet -> Bool
invalidPred InvalidPacket = False
invalidPred _ = True

-- | The main entry point for a protocol.
--   Run this function over a packet and receive zero or more packets in
--   reply. This function should be provided with state so that it can
--   process consecutive packets.
processPacket :: ProtocolState -> Packet -> (ProtocolState, [Packet])

-- | Login. Examine all of the bits, make sure they match, and then reply in
--   kind.
processPacket ps (LoginPacket protocol _ _ _ _ _ _ _) = runST $ do
    state <- newSTRef ps
    packets <- newSTRef []
    -- Is the protocol invalid? Kick the client with an unsupported-protocol
    -- message.
    if (protocol /= 22)
        then do
            modifySTRef state (\x -> x { psStatus = Invalid })
            modifySTRef packets (ErrorPacket (T.pack "Unsupported protocol") :)
        else do
            modifySTRef state (\x -> x { psStatus = Authenticated })
            modifySTRef packets (LoginPacket 1 T.empty 0 Creative Earth Peaceful 128 10 :)
    newps <- readSTRef state
    newpackets <- readSTRef packets
    return (newps, newpackets)

-- | Handshake. Just write down the username.
processPacket ps (HandshakePacket nick) =
    (ps { psNick = nick }, [HandshakePacket $ T.pack "-"])

-- | A poll. Reply with a formatted error packet and close the connection.
processPacket ps PollPacket =
    (ps { psStatus = Invalid },
     [ErrorPacket $ T.pack "Baskerville§0§1", InvalidPacket])

-- | An error on the client side. They have no right to do this, but let them
--   get away with it anyway. They clearly want to be disconnected, so
--   disconnect them.
processPacket ps (ErrorPacket _) = (ps { psStatus = Invalid }, [])

-- | A packet which we don't handle. Kick the client, we're wasting time here.
processPacket ps _ = (ps { psStatus = Invalid }, [])

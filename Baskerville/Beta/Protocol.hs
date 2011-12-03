module Baskerville.Beta.Protocol where

import Data.List
import qualified Data.Text as T

import Baskerville.Beta.Packets

data ProtocolState = ProtocolState ()

-- | A helper for iterating over an infinite packet stream and returning
--   another infinite packet stream in return. When in doubt, use this.
processPacketStream :: [Packet] -> [Packet]
processPacketStream packets =
    let state = ProtocolState ()
        mapper = concat . snd . mapAccumL processPacket state
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
processPacket ps PollPacket =
    (ps, [ErrorPacket $ T.pack "Baskerville§0§1", InvalidPacket])
processPacket ps _ = (ps, [InvalidPacket])

module Baskerville.Beta.Protocol where

import qualified Data.Text as T

import Baskerville.Beta.Packets

process :: Packet -> [Packet]
process PollPacket = [ErrorPacket $ T.pack "Baskerville§0§1"]
process _ = [InvalidPacket]

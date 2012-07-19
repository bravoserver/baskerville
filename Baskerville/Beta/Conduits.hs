module Baskerville.Beta.Conduits where

import qualified Data.ByteString as BS
import Data.Conduit
import Data.Conduit.Cereal
import Data.Serialize

import Baskerville.Beta.Packets

bsToPackets :: MonadThrow m => Conduit BS.ByteString m Packet
bsToPackets = conduitGet get

packetsToBs :: MonadThrow m => Conduit Packet m BS.ByteString
packetsToBs = conduitPut put

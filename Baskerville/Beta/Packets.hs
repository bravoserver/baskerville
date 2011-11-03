module Baskerville.Beta.Packets where

import Data.Attoparsec
-- import qualified Data.ByteString as BS
import Data.Word

data Packet = PollPacket
            | InvalidPacket
            deriving (Show)

parsePacket :: Parser Packet
parsePacket = do
    header <- anyWord8
    packetBody header

packetBody :: Word8 -> Parser Packet
packetBody 0xef = return PollPacket
packetBody _ = return InvalidPacket

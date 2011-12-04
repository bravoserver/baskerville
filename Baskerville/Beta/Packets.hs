module Baskerville.Beta.Packets where

import Prelude hiding (take)

import Data.Attoparsec
import Data.Bits
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Data.Text.Encoding
import Data.Word

-- | The packet datatype.
--   Packets are the basic unit of communication between MC clients and
--   servers. They are atomic and self-contained. The first byte of a packet
--   identifies the packet, and the payload is immediately inlined, with no
--   delimiters. This makes packets difficult to parse.
data Packet = PingPacket Int
            | PollPacket
            | ErrorPacket T.Text
            | InvalidPacket
            deriving (Show)

-- | Parse two bytes and return them as a big-endian short integer.
pWord16 :: Parser Word16
pWord16 = let promote a = fromIntegral a :: Word16
    in do
        b1 <- anyWord8
        b2 <- anyWord8
        return $! (promote b1 `shiftL` 8) .|. promote b2

-- | Pack a big-endian short.
bWord16 :: Integral a => a -> BS.ByteString
bWord16 w = let promote a = fromIntegral a :: Word8
    in BS.pack [promote w `shiftR` 8, promote w]

pWord32 :: Parser Word32
pWord32 = let promote a = fromIntegral a :: Word32
    in do
        b1 <- anyWord8
        b2 <- anyWord8
        b3 <- anyWord8
        b4 <- anyWord8
        return $! (promote b1 `shiftL` 24) .|. (promote b2 `shiftL` 16) .|.
                  (promote b3 `shiftL` 8) .|. promote b4

bWord32 :: Integral a => a -> BS.ByteString
bWord32 w = let promote a = fromIntegral a :: Word8
    in BS.pack [promote w `shiftR` 24, promote w `shiftR` 16,
                promote w `shiftR` 8, promote w]

-- | Parse a length-prefixed UCS2 string and return it as a Text.
pUcs2 :: Parser T.Text
pUcs2 = do
    len <- pWord16
    bytes <- take (fromIntegral len * 2)
    return $ decodeUtf16BE bytes

-- | Pack a text string into a UCS2 length-prefixed string.
bUcs2 :: T.Text -> BS.ByteString
bUcs2 t = BS.append (bWord16 $ T.length t) (encodeUtf16BE t)

parsePackets :: Parser [Packet]
parsePackets = many1 parsePacket

parsePacket :: Parser Packet
parsePacket = do
    header <- anyWord8
    pPacketBody header

pPacketBody :: Word8 -> Parser Packet
pPacketBody 0xfe = return PollPacket
pPacketBody 0xff = do
    message <- pUcs2
    return $! ErrorPacket message
pPacketBody _ = return InvalidPacket

buildPacket :: Packet -> BS.ByteString
buildPacket PollPacket = BS.singleton 0xfe
buildPacket (ErrorPacket t) = BS.cons 0xff (bUcs2 t)
buildPacket _ = BS.empty

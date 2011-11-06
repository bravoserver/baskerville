module Baskerville.Beta.Packets where

import Prelude hiding (take)

import Data.Attoparsec
import Data.Bits
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Data.Text.Encoding
import Data.Word

-- | Parse two bytes and return them as a big-endian short integer.
word16 :: Parser Word16
word16 = let promote a = fromIntegral a :: Word16
    in do
        b1 <- anyWord8
        b2 <- anyWord8
        return $! (promote b1 `shiftL` 8) .|. promote b2

-- | Parse a length-prefixed UCS2 string and return it as a Text.
ucs2 :: Parser T.Text
ucs2 = do
    len <- word16
    bytes <- take (fromIntegral len * 2)
    return $ decodeUtf16BE bytes

data Packet = PollPacket
            | ErrorPacket T.Text
            | InvalidPacket
            deriving (Show)

getPackets :: BS.ByteString -> [Packet]
getPackets bs = case parse parsePacket bs of
    Fail _ _ error -> []
    Partial _ -> []
    Done leftovers packet ->
        if leftovers == BS.empty then [packet] else packet : getPackets leftovers

parsePacket :: Parser Packet
parsePacket = do
    header <- anyWord8
    packetBody header

packetBody :: Word8 -> Parser Packet
packetBody 0xef = return PollPacket
packetBody 0xff = do
    message <- ucs2
    return $! ErrorPacket message
packetBody _ = return InvalidPacket

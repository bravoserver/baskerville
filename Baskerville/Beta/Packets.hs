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
        return $ (promote b1 `shiftL` 8) .&. promote b2

-- | Notchian string.
data UCS2 = UCS2 BS.ByteString

str2ucs :: T.Text -> UCS2
str2ucs s = UCS2 (encodeUtf16BE s)
ucs2str :: UCS2 -> T.Text
ucs2str (UCS2 s) = decodeUtf16BE s

data Packet = PollPacket
            | ErrorPacket T.Text
            | InvalidPacket
            deriving (Show)

parsePacket :: Parser Packet
parsePacket = do
    header <- anyWord8
    packetBody header

packetBody :: Word8 -> Parser Packet
packetBody 0xef = return PollPacket
packetBody 0xff = do
    len <- word16
    message <- take (fromIntegral len * 2)
    return $ ErrorPacket (ucs2str (UCS2 message))
packetBody _ = return InvalidPacket

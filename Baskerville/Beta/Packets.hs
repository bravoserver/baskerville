module Baskerville.Beta.Packets where

import Prelude hiding (take)

import Data.Attoparsec
import qualified Data.ByteString as BS
import Data.Serialize
import qualified Data.Text as T
import Data.Text.Encoding
import Data.Word

data Mode = Survival | Creative deriving (Enum, Show)

instance Serialize Mode where
    put Survival = putWord8 0x00
    put Creative = putWord8 0x01

    get = do
        value <- getWord8
        return $ case value of
            0x01 -> Survival
            _ -> Creative

data Dimension = Earth | Sky | Nether deriving (Enum, Show)

instance Serialize Dimension where
    put Earth = putWord8 0x00
    put Sky = putWord8 0x01
    put Nether = putWord8 0xff

    get = do
        value <- getWord8
        return $ case value of
            0x01 -> Sky
            0xff -> Nether
            _ -> Earth

data DiggingState = Started | Digging | Stopped | Broken | Dropped | Shooting
    deriving (Enum, Show)

data Face = Noop | YMinus | YPlus | ZMinus | ZPlus | XMinus | XPlus
    deriving (Enum, Show)

data Item = Item Word16 Word16 Word8
    deriving (Show)

-- | The packet datatype.
--   Packets are the basic unit of communication between MC clients and
--   servers. They are atomic and self-contained. The first byte of a packet
--   identifies the packet, and the payload is immediately inlined, with no
--   delimiters. This makes packets difficult to parse.
data Packet = PingPacket Word32
            | LoginPacket Word32 T.Text Word64 Mode Dimension Word8 Word8 Word8
            | HandshakePacket T.Text
            | ChatPacket T.Text
            | TimePacket Word64
            | EquipmentPacket Word32 Word16 Word16 Word16
            | SpawnPacket Word32 Word32 Word32
            | UsePacket Word32 Word32 Word8
            | RespawnPacket Dimension Word8 Word8 Word16 Word64
            | DiggingPacket DiggingState Word32 Word8 Word32
            | BuildPacket Word32 Word8 Word32 Face Item
            | PollPacket
            | ErrorPacket T.Text
            | InvalidPacket
            deriving (Show)

instance Serialize Packet where
    put (PingPacket pid) = putWord8 0x00 >> putWord32be pid
    put (HandshakePacket t) = putWord8 0x02 >> putByteString (bUcs2 t)
    put PollPacket = putWord8 0xfe
    put (ErrorPacket t) = putWord8 0xff >> putByteString (bUcs2 t)
    put _ = putByteString BS.empty

    get = return InvalidPacket

-- | Parse two bytes and return them as a big-endian short integer.
pWord16 :: Parser Word16
pWord16 = do
    bytes <- take 2
    return $! case decode bytes of
        Left _ -> 0
        Right x -> x

pWord32 :: Parser Word32
pWord32 = do
    bytes <- take 4
    return $! case decode bytes of
        Left _ -> 0
        Right x -> x

pWord64 :: Parser Word64
pWord64 = do
    bytes <- take 8
    return $! case decode bytes of
        Left _ -> 0
        Right x -> x

-- | Pack a big-endian short.
bWord16 :: Word16 -> BS.ByteString
bWord16 = encode

bWord32 :: Word32 -> BS.ByteString
bWord32 = encode

bWord64 :: Word64 -> BS.ByteString
bWord64 = encode

-- | Parse a length-prefixed UCS2 string and return it as a Text.
pUcs2 :: Parser T.Text
pUcs2 = do
    len <- pWord16
    bytes <- take (fromIntegral len * 2)
    return $ decodeUtf16BE bytes

-- | Pack a text string into a UCS2 length-prefixed string.
bUcs2 :: T.Text -> BS.ByteString
bUcs2 t = BS.append (bWord16 $ fromIntegral (T.length t)) (encodeUtf16BE t)

parsePackets :: Parser [Packet]
parsePackets = many1 parsePacket

parsePacket :: Parser Packet
parsePacket = do
    header <- anyWord8
    pPacketBody header

pPacketBody :: Word8 -> Parser Packet
pPacketBody 0x00 = do
    pid <- pWord32
    return $! PingPacket pid
pPacketBody 0x02 = do
    message <- pUcs2
    return $! HandshakePacket message
pPacketBody 0xfe = return PollPacket
pPacketBody 0xff = do
    message <- pUcs2
    return $! ErrorPacket message
pPacketBody _ = return InvalidPacket

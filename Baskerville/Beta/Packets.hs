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
    -- Stubbed
    get = getWord8 >> return Creative

pMode :: Parser Mode
pMode = do
    value <- anyWord8
    return $ case value of
        0x00 -> Survival
        _ -> Creative

data Dimension = Earth | TheEnd | Nether deriving (Enum, Show)

instance Serialize Dimension where
    put Earth = putWord8 0x00
    put TheEnd = putWord8 0x01
    put Nether = putWord8 0xff
    -- Stubbed
    get = getWord8 >> return Earth

pDimension :: Parser Dimension
pDimension = do
    value <- anyWord8
    return $ case value of
        0x01 -> TheEnd
        0xff -> Nether
        _ -> Earth

data Difficulty = Peaceful | Easy | Medium | Hard deriving (Enum, Show)

instance Serialize Difficulty where
    put Peaceful = putWord8 0x00
    put Easy = putWord8 0x01
    put Medium = putWord8 0x02
    put Hard = putWord8 0x03
    -- Stubbed
    get = getWord8 >> return Peaceful

pDifficulty :: Parser Difficulty
pDifficulty = do
    value <- anyWord8
    return $ case value of
        0x01 -> Easy
        0x02 -> Medium
        0x03 -> Hard
        _ -> Peaceful

data DiggingState = Started | Digging | Stopped | Broken | Dropped | Shooting
    deriving (Enum, Show)

data Face = Noop | YMinus | YPlus | ZMinus | ZPlus | XMinus | XPlus
    deriving (Enum, Show)

data Item = Item { primary :: Word16, secondary :: Word16, quantity :: Word8 }
    deriving (Show)

instance Serialize Item where
    put (Item p s q) = putWord16be p >> putWord16be s >> putWord8 q
    get = do
        p <- getWord16be
        s <- getWord16be
        q <- getWord8
        return $ Item p s q

-- | The packet datatype.
--   Packets are the basic unit of communication between MC clients and
--   servers. They are atomic and self-contained. The first byte of a packet
--   identifies the packet, and the payload is immediately inlined, with no
--   delimiters. This makes packets difficult to parse.
data Packet = PingPacket Word32
            | LoginPacket Word32 T.Text Word64 Mode Dimension Difficulty Word8 Word8
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
    put (LoginPacket a b c d e f g h) = do
        putWord8 0x01
        putWord32be a
        putUcs2 b
        putWord64be c
        put d
        put e
        put f
        put g
        put h
    put (HandshakePacket t) = putWord8 0x02 >> putUcs2 t
    put PollPacket = putWord8 0xfe
    put (ErrorPacket t) = putWord8 0xff >> putUcs2 t
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

-- | Parse a length-prefixed UCS2 string and return it as a Text.
pUcs2 :: Parser T.Text
pUcs2 = do
    len <- pWord16
    bytes <- take (fromIntegral len * 2)
    return $ decodeUtf16BE bytes

-- | Pack a text string into a UCS2 length-prefixed string.
bUcs2 :: T.Text -> BS.ByteString
bUcs2 t = BS.append (bWord16 $ fromIntegral (T.length t)) (encodeUtf16BE t)

putUcs2 :: Putter T.Text
putUcs2 = putByteString . bUcs2

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
pPacketBody 0x01 = do
    protocol <- pWord32
    challenge <- pUcs2
    seed <- pWord64
    mode <- pMode
    dimension <- pDimension
    difficulty <- pDifficulty
    height <- anyWord8
    players <- anyWord8
    return $! LoginPacket protocol challenge seed mode dimension difficulty
        height players
pPacketBody 0x02 = do
    message <- pUcs2
    return $! HandshakePacket message
pPacketBody 0xfe = return PollPacket
pPacketBody 0xff = do
    message <- pUcs2
    return $! ErrorPacket message
pPacketBody _ = return InvalidPacket

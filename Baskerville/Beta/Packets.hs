module Baskerville.Beta.Packets where

import qualified Data.ByteString as BS
import Data.Serialize
import qualified Data.Text as T
import Data.Text.Encoding
import Data.Word

-- From edwardk's stash-o-stuff.
infixl 4 <$!>

(<$!>) :: Monad m => (a -> b) -> m a -> m b
f <$!> ma = do
    a <- ma
    return $! f a

data Mode = Survival | Creative
    deriving (Enum, Eq, Show)

instance Serialize Mode where
    put Survival = putWord8 0x00
    put Creative = putWord8 0x01
    -- Default to Creative.
    get = do
        m <- getWord8
        return $ case m of
            0x00 -> Survival
            _    -> Creative

data Dimension = Earth | TheEnd | Nether
    deriving (Enum, Eq, Show)

instance Serialize Dimension where
    put Earth = putWord8 0x00
    put TheEnd = putWord8 0x01
    put Nether = putWord8 0xff
    -- Default to Earth.
    get = do
        d <- getWord8
        return $ case d of
            0x01 -> TheEnd
            0xff -> Nether
            _    -> Earth

data Difficulty = Peaceful | Easy | Medium | Hard
    deriving (Enum, Eq, Show)

instance Serialize Difficulty where
    put Peaceful = putWord8 0x00
    put Easy = putWord8 0x01
    put Medium = putWord8 0x02
    put Hard = putWord8 0x03
    -- Default to Peaceful.
    get = do
        d <- getWord8
        return $ case d of
            0x01 -> Easy
            0x02 -> Medium
            0x03 -> Hard
            _    -> Peaceful

data Airborne = Grounded | Aloft
    deriving (Enum, Eq, Show)

instance Serialize Airborne where
    put Grounded = putWord8 0x00
    put Aloft = putWord8 0x01
    -- Default to Aloft, since it's a boolean.
    get = do
        d <- getWord8
        return $ case d of
            0x00 -> Grounded
            _    -> Aloft

data DiggingState = Started | Digging | Stopped | Broken | Dropped | Shooting
    deriving (Enum, Eq, Show)

data Face = Noop | YMinus | YPlus | ZMinus | ZPlus | XMinus | XPlus
    deriving (Enum, Eq, Show)

data Item = Item { primary :: Word16, secondary :: Word16, quantity :: Word8 }
    deriving (Eq, Show)

instance Serialize Item where
    put (Item p s q) = putWord16be p >> putWord16be s >> putWord8 q
    get = do
        p <- getWord16be
        s <- getWord16be
        q <- getWord8
        return $ Item p s q

-- | Pack a text string into a UCS2 length-prefixed string.
bUcs2 :: T.Text -> BS.ByteString
bUcs2 t = BS.append (bWord16 $ fromIntegral (T.length t)) (encodeUtf16BE t)

-- | Pack a big-endian short.
bWord16 :: Word16 -> BS.ByteString
bWord16 = encode

putUcs2 :: Putter T.Text
putUcs2 = putByteString . bUcs2

-- | Parse a length-prefixed UCS2 string and return it as a Text.
getUcs2 :: Get T.Text
getUcs2 = do
    len <- getWord16be
    bytes <- getByteString (fromIntegral len * 2)
    return $ decodeUtf16BE bytes

-- | The packet datatype.
--   Packets are the basic unit of communication between MC clients and
--   servers. They are atomic and self-contained. The first byte of a packet
--   identifies the packet, and the payload is immediately inlined, with no
--   delimiters. This makes packets difficult to parse.
data Packet = PingPacket Word32
            | LoginPacket Word32 T.Text T.Text Mode Dimension Difficulty Word8 Word8
            | HandshakePacket T.Text
            | ChatPacket T.Text
            | TimePacket Word64
            | EquipmentPacket Word32 Word16 Word16 Word16
            | SpawnPacket Word32 Word32 Word32
            | UsePacket Word32 Word32 Word8
            | RespawnPacket Dimension Difficulty Mode Word16 T.Text
            | AirbornePacket Airborne
            | DiggingPacket DiggingState Word32 Word8 Word32
            | BuildPacket Word32 Word8 Word32 Face Item
            | PollPacket
            | ErrorPacket T.Text
            | InvalidPacket
    deriving (Eq, Show)

instance Serialize Packet where
    put (PingPacket pid) = putWord8 0x00 >> put pid
    put (LoginPacket a b c d e f g h) = do
        putWord8 0x01
        put a
        putUcs2 b
        putUcs2 c
        put d
        put e
        put f
        put g
        put h
    put (HandshakePacket t) = putWord8 0x02 >> putUcs2 t
    put (ChatPacket t) = putWord8 0x03 >> putUcs2 t
    put (TimePacket t) = putWord8 0x04 >> put t
    put (AirbornePacket a) = putWord8 0x0a >> put a
    put PollPacket = putWord8 0xfe
    put (ErrorPacket t) = putWord8 0xff >> putUcs2 t
    put _ = putByteString BS.empty

    get = do
        header <- getWord8
        case header of
            0x00 -> PingPacket <$!> get
            0x01 -> getLoginPacket
            0x02 -> HandshakePacket <$!> getUcs2
            0x03 -> ChatPacket <$!> getUcs2
            0x04 -> TimePacket <$!> get
            0xfe -> return PollPacket
            0xff -> ErrorPacket <$!> getUcs2
            _    -> return InvalidPacket

getLoginPacket :: Get Packet
getLoginPacket = do
    protocol <- get
    username <- getUcs2
    levelType <- getUcs2
    mode <- get
    dimension <- get
    difficulty <- get
    unused <- get
    players <- get
    return $! LoginPacket protocol username levelType mode dimension difficulty
        unused players

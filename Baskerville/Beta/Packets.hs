module Baskerville.Beta.Packets where

import Control.Applicative
import Control.Monad
import qualified Data.ByteString as BS
import Data.Bits
import Data.Serialize
import qualified Data.Text as T
import Data.Text.Encoding
import Data.Word
import Debug.Trace

import Baskerville.Coords
import Baskerville.Chunk
import Baskerville.Location

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

data Face = YMinus | YPlus | ZMinus | ZPlus | XMinus | XPlus
    deriving (Enum, Eq, Show)

instance Serialize Face where
    put = putWord8 . toEnum . fromEnum
    get = fmap (toEnum . fromEnum) getWord8

data Item = Item { primary :: Word16, secondary :: Word16, quantity :: Word8 }
    deriving (Eq, Show)

instance Serialize Item where
    put (Item p s q) = putWord16be p >> putWord16be s >> putWord8 q
    get = do
        p <- getWord16be
        s <- getWord16be
        q <- getWord8
        return $ Item p s q

-- | Player animations
data Animation = NoAnim | SwingArmAnim | DamageAnim | ExitBedAnim | EatAnim | CrouchAnim | UncrouchAnim
    deriving (Eq, Show)

instance Serialize Animation where
    put NoAnim = putWord8 0x00
    put SwingArmAnim = putWord8 0x01
    put DamageAnim = putWord8 0x02
    put ExitBedAnim = putWord8 0x03
    put EatAnim = putWord8 0x05
    put CrouchAnim = putWord8 0x68
    put UncrouchAnim = putWord8 0x69
    get = do
        d <- getWord8
        return $ case d of
            0x01 -> SwingArmAnim
            0x02 -> DamageAnim
            0x03 -> ExitBedAnim
            0x05 -> EatAnim
            0x68 -> CrouchAnim
            0x69 -> UncrouchAnim
            _    -> NoAnim

-- | Player Actions
data Action = CrouchAct | UncrouchAct | ExitBedAct | SprintAct | UnSprintAct
    deriving (Eq, Show)

instance Serialize Action where
    put CrouchAct = putWord8 0x01
    put UncrouchAct = putWord8 0x02
    put ExitBedAct = putWord8 0x03
    put SprintAct = putWord8 0x04
    put UnSprintAct = putWord8 0x05
    get = do
        d <- getWord8
        return $ case d of
            0x01 -> CrouchAct
            0x02 -> UncrouchAct
            0x03 -> ExitBedAct
            0x04 -> SprintAct
            0x05 -> UnSprintAct
            -- We choose to make uncrouching the default, since it is nearly
            -- always harmless.
            _    -> UncrouchAct

-- | Objects and Vehicles
data ObVehicle = BoatOb
               | MinecartOb
               | MinecartStorageOb
               | MinecartPoweredOb
               | TNTOb
               | EnderCrystalOb
               | ArrowOb
               | SnowballOb
               | EggOb
               | SandOb
               | GravelOb
               | EyeOfEnderOb
               | DragonEggOb
               | FishingFloatOb
    deriving (Eq, Show)

-- | ObVehicle probably needs a rename, but more importantly...serialization

data PaintDirection = PPosX | PNegX | PPosZ | PNegZ
    deriving (Eq, Show)

instance Serialize PaintDirection where
    put PPosX = putWord32be 0x03
    put PNegX = putWord32be 0x01
    put PPosZ = putWord32be 0x02
    put PNegZ = putWord32be 0x00
    get = do
        d <- getWord32be
        return $ case d of
            0x03 -> PPosX
            0x02 -> PPosZ
            0x01 -> PNegX
            _    -> PNegZ

-- | Newtype for EIDs.
newtype EID = EID { unEID :: Word32 }
    deriving (Eq, Show)

-- Just write out the instance by hand; it's not that big of a deal and GNTD
-- is not safe.
instance Serialize EID where
    put (EID eid) = put eid
    get = EID <$!> get

-- | Put an arbitrary-length integer.
putInteger :: Putter Integer
putInteger i = let ws = chop i in do
    mapM_ (putWord8 . (.|. 0x80)) $ init ws
    putWord8 $ last ws
    where
    chop :: Integer -> [Word8]
    chop i = if i >= 0x80
        then fromInteger (i .&. 0x7f) : chop (i `shiftR` 7)
        else [fromInteger i]

-- | Get an arbitrary-length integer.
getInteger :: Get Integer
getInteger = do
    bs <- loop
    return $! intify bs
    where
    loop = do
        b <- getWord8
        if b >= 0x80
            then do
                bs <- loop
                -- Clear off the continuation bit.
                return $ (b .&. 0x7f) : bs
            else return [b]
    -- And there are only seven bits here, so shift by seven, not eight.
    intify = foldr (\b i -> (i `shiftL` 7) .|. toInteger b) 0

-- | Put a Text as a UTF-8 length-prefixed string.
putText :: Putter T.Text
putText text = do
    putInteger . toInteger . T.length $ text
    putByteString . encodeUtf8 $ text

-- | Get a Text as a UTF-8 length-prefixed string.
getText :: Get T.Text
getText = do
    len <- getInteger
    bytes <- getByteString $ fromIntegral len
    return $ decodeUtf8 bytes

-- | Pack a big-endian short.
bWord16 :: Word16 -> BS.ByteString
bWord16 = encode

-- | Pack a text string into a UCS2 length-prefixed string.
bUcs2 :: T.Text -> BS.ByteString
bUcs2 t = let len = bWord16 $ fromIntegral (T.length t)
    in BS.append len (encodeUtf16BE t)

putUcs2 :: Putter T.Text
putUcs2 = putByteString . bUcs2

-- | Parse a length-prefixed UCS2 string and return it as a Text.
getUcs2 :: Get T.Text
getUcs2 = do
    len <- getWord16be
    bytes <- getByteString (fromIntegral len * 2)
    return $ decodeUtf16BE bytes

-- | Put a packet, calculating the size and putting the header in front of it.
--   Note that we assume that packet indices are always Word8. This is largely
--   because there are no packets that are any other size; we don't really
--   want to spend the extra effort manipulating Integers if there's no
--   justification for it.
putPacketHeader :: Word8 -> Put -> Put
putPacketHeader index pput =
    let packet = runPut pput
        len    = toInteger $ 1 + BS.length packet
    in do
        putInteger len
        putWord8 index
        putByteString packet

-- | Incoming packets.
--   These packets can only be sent by clients.
data IncomingPacket = ClientSettings T.Text Word8 Word8 Difficulty Bool
                    | PluginMessage T.Text BS.ByteString
    deriving (Eq, Show)

getPacket :: Get IncomingPacket
getPacket = do
    -- We really don't care about the length of the packet. Really.
    void getInteger
    header <- getInteger
    case header of
        0x15 -> do
            locale <- getText
            distance <- getWord8
            chat <- getWord8
            -- Unused value in the middle of the packet.
            void getWord8
            difficulty <- get
            cape <- get
            return $! ClientSettings locale distance chat difficulty cape
        0x17 -> do
            channel <- getText
            len <- getWord16be
            bytes <- getByteString $ fromIntegral len
            return $! PluginMessage channel bytes
        _    -> error $ "Can't decode packet with header " ++ show header

-- | Outgoing packets.
--   These packets can only be sent by servers.
data OutgoingPacket = Join EID Mode Dimension Difficulty Word8 T.Text
    deriving (Eq, Show)

putPacket :: Putter OutgoingPacket
putPacket (Join eid mode dimension difficulty players level) =
    putPacketHeader 0x01 $ do
        put eid
        put mode
        put dimension
        put difficulty
        put players
        putText level

-- | The (old) packet datatype.
--   Packets are the basic unit of communication between MC clients and
--   servers. They are atomic and self-contained. The first byte of a packet
--   identifies the packet, and the payload is immediately inlined, with no
--   delimiters. This makes packets difficult to parse.
data Packet = Ping Word32 -- 0x00
            | Login EID T.Text Mode Dimension Difficulty Word8 -- 0x01
            -- | Handshake Word8 T.Text T.Text Word32 -- 0x02
            | Chat T.Text -- 0x03
            | Time Word64 Word64 -- 0x04
            | Equipment EID Word16 Word16 Word16 -- 0x05
            | Spawn BCoord -- 0x06
            | Use EID EID Bool -- 0x07
            | UpdateHealth Word16 Word16 Float -- 0x08
            | Respawn Dimension Difficulty Mode Word16 T.Text -- 0x09
            | AirbornePacket Airborne -- 0x0A
            | PositionPacket Position Airborne -- 0x0B
            | OrientationPacket Orientation Airborne -- 0x0C
            | LocationPacket Position Orientation Airborne -- 0x0D
            | Dig DiggingState Word32 Word8 Word32 Face -- 0x0E
            -- | PlaceBlock WorldDirection Word8 Word32 Word8 Slot -- 0x0F
            | SlotSelection Word16 -- 0x10
            | UseBed EID Word8 Word32 Word8 Word32 -- 0x11
            | Animation EID Animation -- 0x12
            | Action EID Action -- 0x13
            | SpawnNamed EID T.Text Word32 Word32 Word32 Word8 Word8 Item -- 0x14
            | SpawnDropped EID Item Word8 Word16 Word32 Word32 Word32 Word8 Word8 Word8 -- 0x15
            | CollectItem EID EID -- 0x16
            -- Two 0x17 possibilities
            -- | SpawnObject Word32 ObVehicle Word32 Word32 Word32 Word32 -- 0x17
            -- | SpawnObject Word32 ObVehicle Word32 Word32 Word32 Word32 Word16 Word16 Word16 -- 0x17
            -- | SpawnMob Word32 Mob Word32 Word32 Word32 Word8 Word8 Word8 Metadata -- 0x18
            | SpawnPainting EID T.Text BCoord PaintDirection -- 0x19
            | SpawnExperience EID Word32 Word32 Word32 Word16 -- 0x1A
            | Velocity EID Word16 Word16 Word16 -- 0x1C
            | DestroyEntity [EID] -- 0x1D
            | Entity EID -- 0x1E
            | RelativeMove EID Word8 Word8 Word8 -- 0x1F
            | EntityLook EID Word8 Word8 -- 0x20
            | EntityLookMove EID Word8 Word8 Word8 Word8 Word8 -- 0x21
            | EntityTeleport EID Word32 Word32 Word32 Word8 Word8 -- 0x22
            | EntityHeadLook EID Word8 -- 0x23
            -- | EntityStatus EID Status -- 0x26
            | EntityAttach EID Word32 -- 0x27
            -- | EntityMetadata EID Metadata -- 0x28
            | EntityEffect EID Word8 Word8 Word16 -- 0x29
            | EndEntityEffect EID Word8 -- 0x2A
            | SetExperience Float Word16 Word16 -- 0x2B
            | AllocChunk Word32 Word32 Bool -- 0x32
            | SendChunk Chunk -- 0x33
            -- | MultiBlockChange Word32 Word32 Word16 Word32 MultiBlockData -- 0x34
            | BlockChange Word32 Word8 Word32 Word8 Word8 -- 0x35
            -- | BlockAction Word32 Word16 Word32 BlockAction BlockAction -- 0x36
            -- | Explosion Double Double Double Float Word32 [Record] -- 0x3C
            -- | SoundParticleEffect Word32 Word32 Word8 Word32 EffectData -- 0x3D
            -- | GameStateChange Reason Difficulty -- 0x46
            | Thunderbolt EID Word8 Word32 Word32 Word32 -- 0x47
            | WindowOpen Word8 Word8 T.Text Word8 -- 0x64
            | WindowClose Word8 -- 0x65
            -- | WindowClick Word8 Word16 Word8 Word16 Bool Slot -- 0x66
            -- | WindowSlotChange Word8 Word16 Slot -- 0x67
            -- | SetWindowItems Word8 Word16 [Slot] -- 0x68
            -- | UpdateWindow Word8 WindowProperty Word16 -- 0x69
            -- | Confirmation Word8 Word16 Accepted -- 0x6A
            -- | CreativeInventoryAction Word16 Slot -- 0x6B
            | EnchantItem Word8 Word8 -- 0x6C
            | UpdateSign Word32 Word16 Word32 T.Text T.Text T.Text T.Text -- 0c82
            -- | ItemData Item Word16 Word8 [Word8] -- 0x83
            -- | UpdateOnTileEntity Word32 Word16 Word32 UpdateType Word32 Word32 Word32 -- 0x84
            -- | AlterStat Stat Word8 -- 0xC8
            | PlayerList T.Text Bool Word16 -- 0xC9
            -- | PlayerAbilities Invulnerable Flying Flyable InstaBreak -- 0xCA
            | Poll
            | Error T.Text
            | InvalidPacket
    deriving (Eq, Show)

instance Serialize Packet where
    put (Ping pid) = putWord8 0x00 >> put pid
    put (Login a b c d e f) = do
        putWord8 0x01
        put a
        putUcs2 b
        putWord32be $ (toEnum . fromEnum) c
        putWord32be $ (toEnum . fromEnum) d
        put e
        putWord8 0x00 -- Unused field, should always be 0x0
        put f
    put (Chat t) = putWord8 0x03 >> putUcs2 t
    put (Time a t) = putWord8 0x04 >> put a >> put t
    put (Spawn c) = putWord8 0x06 >> put c
    put (AirbornePacket a) = putWord8 0x0a >> put a
    put (PositionPacket p a) = putWord8 0x0b >> put p >> put a
    put (OrientationPacket o a) = putWord8 0x0c >> put o >> put a
    put (LocationPacket p o a) = putWord8 0x0d >> put p >> put o >> put a
    put (SendChunk c) = putWord8 0x33 >> put c
    put Poll = putWord8 0xfe
    put (Error t) = putWord8 0xff >> putUcs2 t
    put p = error $ "Won't put packet " ++ show p

    get = do
        header <- getWord8
        case header of
            0x00 -> Ping <$!> get
            -- 0x01 S->C only
            -- 0x02 -> do
            --     protocol <- getWord8
            --     username <- getUcs2
            --     host <- getUcs2
            --     port <- get
            --     return $! Handshake protocol username host port
            0x03 -> Chat <$!> getUcs2
            -- 0x04 S->C only
            0x06 -> Spawn <$!> get
            0x0a -> AirbornePacket <$!> get
            0x0b -> PositionPacket <$!> get <*> get
            0x0c -> OrientationPacket <$!> get <*> get
            0x0d -> LocationPacket <$!> get <*> get <*> get
            0x10 -> SlotSelection <$!> get
            -- 0xfe should always be followed by 0x01, not by anything.
            -- Nonetheless, I'm going to just not bother checking. Whatever,
            -- yo. ~ C.
            0xfe -> getWord8 >> return Poll
            0xff -> Error <$!> getUcs2
            _    -> do
                let s = "Can't decode packet " ++ show header
                trace s $ return InvalidPacket

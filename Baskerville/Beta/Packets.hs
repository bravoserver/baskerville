module Baskerville.Beta.Packets where

import Control.Applicative
import qualified Data.ByteString as BS
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

-- | Pack a big-endian short.
bWord16 :: Word16 -> BS.ByteString
bWord16 = encode

-- | Pack a text string into a UCS2 length-prefixed string.
bUcs2 :: T.Text -> BS.ByteString
bUcs2 t = BS.append (bWord16 $ fromIntegral (T.length t)) (encodeUtf16BE t)

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
data Packet = PingPacket Word32 -- 0x00
            | LoginPacket Word32 T.Text Mode Dimension Difficulty Word8 -- 0x01
            | HandshakePacket T.Text T.Text Word32 -- 0x02
            | ChatPacket T.Text -- 0x03
            | TimePacket Word64 Word64 -- 0x04
            | EquipmentPacket Word32 Word16 Word16 Word16 -- 0x05
            | SpawnPacket BCoord -- 0x06
            | UsePacket Word32 Word32 Bool -- 0x07
            | UpdateHealth Word16 Word16 Float -- 0x08
            | RespawnPacket Dimension Difficulty Mode Word16 T.Text -- 0x09
            | AirbornePacket Airborne -- 0x0A | Known as Player on kev009
            | PositionPacket Position Airborne -- 0x0B
            | OrientationPacket Orientation Airborne -- 0x0C
            | LocationPacket Position Orientation Airborne -- 0x0D
            | DiggingPacket DiggingState Word32 Word8 Word32 Face -- 0x0E
            -- | PlaceBlockPacket WorldDirection Word8 Word32 Word8 Slot -- 0x0F
            | SlotSelectionPacket Word16 -- 0x10
            | UseBedPacket Word32 Word8 Word32 Word8 Word32 -- 0x11
            | AnimationPacket Word32 Animation -- 0x12
            | ActionPacket Word32 Action -- 0x13
            | SpawnNamedPacket Word32 T.Text Word32 Word32 Word32 Word8 Word8 Item -- 0x14
            | SpawnDroppedPacket Word32 Item Word8 Word16 Word32 Word32 Word32 Word8 Word8 Word8 -- 0x15
            | CollectItemPacket Word32 Word32 -- 0x16
            -- Two 0x17 possibilities
            -- | SpawnObjectPacket Word32 ObVehicle Word32 Word32 Word32 Word32 -- 0x17
            -- | SpawnObjectPacket Word32 ObVehicle Word32 Word32 Word32 Word32 Word16 Word16 Word16 -- 0x17
            -- | SpawnMobPacket Word32 Mob Word32 Word32 Word32 Word8 Word8 Word8 Metadata -- 0x18
            | SpawnPaintingPacket Word32 T.Text BCoord PaintDirection -- 0x19
            | SpawnExperiencePacket Word32 Word32 Word32 Word32 Word16 -- 0x1A
            | VelocityPacket Word32 Word16 Word16 Word16 -- 0x1C
            | DestroyEntityPacket Word32 -- 0x1D
            | EntityPacket Word32 -- 0x1E
            | RelativeMovePacket Word32 Word8 Word8 Word8 -- 0x1F
            | EntityLookPacket Word32 Word8 Word8 -- 0x20
            | EntityLookMovePacket Word32 Word8 Word8 Word8 Word8 Word8 -- 0x21
            | EntityTeleportPacket Word32 Word32 Word32 Word32 Word8 Word8 -- 0x22
            | EntityHeadLookPacket Word32 Word8 -- 0x23
            -- | EntityStatus Word32 Status -- 0x26
            | EntityAttachPacket Word32 Word32 -- 0x27
            -- | EntityMetadataPacket Word32 Metadata -- 0x28
            | EntityEffectPacket Word32 Word8 Word8 Word16 -- 0x29
            | EndEntityEffectPacket Word32 Word8 -- 0x2A
            | SetExperiencePacket Float Word16 Word16 -- 0x2B
            | AllocChunkPacket Word32 Word32 Bool -- 0x32
            | ChunkPacket Chunk -- 0x33
            -- | MultiBlockChangePacket Word32 Word32 Word16 Word32 MultiBlockData -- 0x34
            | BlockChangePacket Word32 Word8 Word32 Word8 Word8 -- 0x35
            -- | BlockActionPacket Word32 Word16 Word32 BlockAction BlockAction -- 0x36
            -- | ExplosionPacket Double Double Double Float Word32 [Record] -- 0x3C
            -- | SoundParticleEffectPacket Word32 Word32 Word8 Word32 EffectData -- 0x3D
            -- | GameStateChangePacket Reason Difficulty -- 0x46
            | ThunderboltPacket Word32 Word8 Word32 Word32 Word32 -- 0x47
            | WindowOpenPacket Word8 Word8 T.Text Word8 -- 0x64
            | WindowClosePacket Word8 -- 0x65
            -- | WindowClickPacket Word8 Word16 Word8 Word16 Bool Slot -- 0x66
            -- | WindowSlotChangePacket Word8 Word16 Slot -- 0x67
            -- | SetWindowItemsPacket Word8 Word16 [Slot] -- 0x68
            -- | UpdateWindowPacket Word8 WindowProperty Word16 -- 0x69
            -- | ConfirmationPacket Word8 Word16 Accepted -- 0x6A
            -- | CreativeInventoryActionPacket Word16 Slot -- 0x6B
            | EnchantItemPacket Word8 Word8 -- 0x6C
            | UpdateSignPacket Word32 Word16 Word32 T.Text T.Text T.Text T.Text -- 0c82
            -- | ItemDataPacket Item Word16 Word8 [Word8] -- 0x83
            -- | UpdateOnTileEntity Word32 Word16 Word32 UpdateType Word32 Word32 Word32 -- 0x84
            -- | AlterStatPacket Stat Word8 -- 0xC8
            | PlayerListPacket T.Text Bool Word16 -- 0xC9
            -- | PlayerAbilities Invulnerable Flying Flyable InstaBreak -- 0xCA
            -- | PluginMessagePacket T.Text Word16 [Word8] -- 0xFA
            | PollPacket
            | ErrorPacket T.Text
            | InvalidPacket
    deriving (Eq, Show)

instance Serialize Packet where
    put (PingPacket pid) = putWord8 0x00 >> put pid
    put (LoginPacket a b c d e f) = do
        putWord8 0x01
        put a
        putUcs2 b
        putWord32be $ (toEnum . fromEnum) c
        putWord32be $ (toEnum . fromEnum) d
        put e
        putWord8 0x00 -- Unused field, should always be 0x0
        put f
    put hp@(HandshakePacket{}) =
        error $ "Can't put HandshakePacket " ++ show hp
    put (ChatPacket t) = putWord8 0x03 >> putUcs2 t
    put (TimePacket a t) = putWord8 0x04 >> put a >> put t
    put (SpawnPacket c) = putWord8 0x06 >> put c
    put (AirbornePacket a) = putWord8 0x0a >> put a
    put (PositionPacket p a) = putWord8 0x0b >> put p >> put a
    put (OrientationPacket o a) = putWord8 0x0c >> put o >> put a
    put (LocationPacket p o a) = putWord8 0x0d >> put p >> put o >> put a
    put (ChunkPacket c) = putWord8 0x33 >> put c
    put PollPacket = putWord8 0xfe
    put (ErrorPacket t) = putWord8 0xff >> putUcs2 t
    put _ = putByteString BS.empty

    get = do
        header <- getWord8
        case header of
            0x00 -> PingPacket <$!> get
            -- 0x01 S->C only
            0x02 -> do
                _ <- getWord8
                username <- getUcs2
                host <- getUcs2
                port <- get
                return $! HandshakePacket username host port
            0x03 -> ChatPacket <$!> getUcs2
            -- 0x04 S->C only
            0x06 -> SpawnPacket <$!> get
            0x0d -> LocationPacket <$!> get <*> get <*> get
            0x33 -> ChunkPacket <$!> get
            0xfe -> return PollPacket
            0xff -> ErrorPacket <$!> getUcs2
            _    -> do
                let s = "Can't deal with packet " ++ show header
                trace s $ return InvalidPacket

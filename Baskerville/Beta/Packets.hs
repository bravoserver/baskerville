module Baskerville.Beta.Packets where

import Control.Applicative
import Control.Monad
import qualified Data.ByteString as BS
import Data.Bits
import Data.Int
import Data.Serialize
import qualified Data.Text as T
import Data.Text.Encoding
import Data.Word

import Baskerville.Chunk
import Baskerville.Coords

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
    deriving (Enum, Eq, Ord, Show)

instance Serialize Airborne where
    put Grounded = putWord8 0x00
    put Aloft = putWord8 0x01
    -- Default to Aloft, since it's a boolean.
    get = do
        d <- getWord8
        return $ case d of
            0x00 -> Grounded
            _    -> Aloft

data DigStatus = StartDig
               | CancelDig
               | StopDig
               | DropStack
               | DropItem
               | Shoot
    deriving (Enum, Eq, Show)

instance Serialize DigStatus where
    put = putWord8 . toEnum . fromEnum
    get = fmap (toEnum . fromEnum) getWord8

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

data ActionStatus = Respawn | RequestStats | OpenInventory
    deriving (Eq, Show)

instance Serialize ActionStatus where
    put Respawn = putWord8 0x00
    put RequestStats = putWord8 0x01
    put OpenInventory = putWord8 0x02
    get = do
        b <- getWord8
        return $ case b of
            0x00 -> Respawn
            0x01 -> RequestStats
            0x02 -> OpenInventory
            _    -> error "Invalid value for action status"

data Slot = EmptySlot | Slot Word16 Word8 Word16
    deriving (Eq, Show)

instance Serialize Slot where
    put EmptySlot = putWord16be 0xffff
    put (Slot primary count secondary) = do
        putWord16be primary
        putWord8 count
        putWord16be secondary
        -- Stub out NBT data for now.
        putWord16be 0xffff
    get = do
        primary <- getWord16be
        case primary of
            0xffff -> return EmptySlot
            _      -> do
                count <- getWord8
                secondary <- getWord16be
                -- Hope there wasn't NBT data in there!
                void getWord16be
                return $! Slot primary count secondary

-- | Newtype for EIDs.
newtype EID = EID { unEID :: Word32 }
    deriving (Eq, Show)

-- Just write out the instance by hand; it's not that big of a deal and GNTD
-- is not safe.
instance Serialize EID where
    put (EID eid) = put eid
    get = EID <$!> get

newtype WID = WID { unWID :: Word8 }
    deriving (Eq, Show)

instance Serialize WID where
    put (WID wid) = put wid
    get = WID <$!> get

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
data IncomingPacket = Pong Word32
                    | ClientAirborne Airborne
                    | ClientPosition Double Double Double Double Airborne
                    | ClientOrientation Float Float Airborne
                    | ClientLocation Double Double Double Double Float Float Airborne
                    | Dig DigStatus BCoord Face
                    | Build BCoord Face Slot
                    | SelectSlot Word16
                    | ClientAnimation EID Animation
                    | CloseWindow WID
                    | CreativeAction Word16 Slot
                    | ChangeAbilities Word8 Float Float
                    | ClientSettings T.Text Word8 Word8 Difficulty Bool
                    | ClientStatus ActionStatus
                    | PluginMessage T.Text BS.ByteString
    deriving (Eq, Show)

getPacket :: Get IncomingPacket
getPacket = do
    -- We really don't care about the length of the packet. Really.
    void getInteger
    header <- getInteger
    case header of
        0x00 -> Pong <$> getWord32be
        0x03 -> ClientAirborne <$> get
        0x04 -> do
            x <- getFloat64be
            y <- getFloat64be
            stance <- getFloat64be
            z <- getFloat64be
            grounded <- get
            return $! ClientPosition x y stance z grounded
        0x05 -> do
            yaw <- getFloat32be
            pitch <- getFloat32be
            grounded <- get
            return $! ClientOrientation yaw pitch grounded
        0x06 -> do
            x <- getFloat64be
            y <- getFloat64be
            stance <- getFloat64be
            z <- getFloat64be
            yaw <- getFloat32be
            pitch <- getFloat32be
            grounded <- get
            return $! ClientLocation x y stance z yaw pitch grounded
        0x07 -> Dig <$!> get <*> get <*> get
        0x08 -> do
            bcoord <- get
            face <- get
            item <- get
            -- Discard the crosshair positions.
            replicateM_ 3 getWord8
            return $! Build bcoord face item
        0x09 -> SelectSlot <$> get
        0x0a -> ClientAnimation <$> get <*> get
        0x0d -> CloseWindow <$> get
        0x10 -> CreativeAction <$> getWord16be <*> get
        0x13 -> ChangeAbilities <$> get <*> getFloat32be <*> getFloat32be
        0x15 -> do
            locale <- getText
            distance <- getWord8
            chat <- getWord8
            -- Unused value in the middle of the packet.
            void getWord8
            difficulty <- get
            cape <- get
            return $! ClientSettings locale distance chat difficulty cape
        0x16 -> ClientStatus <$> get
        0x17 -> do
            channel <- getText
            len <- getWord16be
            bytes <- getByteString $ fromIntegral len
            return $! PluginMessage channel bytes
        _    -> error $ "Can't decode packet with header " ++ show header

-- | Outgoing packets.
--   These packets can only be sent by servers.
data OutgoingPacket = Ping Word32
                    | Join EID Mode Dimension Difficulty Word8 T.Text
                    | ServerLocation Double Double Double Float Float Airborne
                    | ChunkData Chunk
                    | SingleBlock Int32 Word8 Int32 Integer Word8
                    | Error T.Text
    deriving (Eq, Show)

putPacket :: Putter OutgoingPacket
putPacket (Ping p) = putPacketHeader 0x00 $ putWord32be p
putPacket (Join eid mode dimension difficulty players level) =
    putPacketHeader 0x01 $ do
        put eid
        put mode
        put dimension
        put difficulty
        put players
        putText level
putPacket (ServerLocation x y z yaw pitch grounded) =
    putPacketHeader 0x08 $ do
        putFloat64be x
        putFloat64be y
        putFloat64be z
        putFloat32be yaw
        putFloat32be pitch
        put grounded
putPacket (ChunkData chunk) = putPacketHeader 0x21 $ putChunk chunk
putPacket (SingleBlock x y z p s) = putPacketHeader 0x23 $ do
    put x
    put y
    put z
    putInteger p
    put s
putPacket (Error message) = putPacketHeader 0x40 $ putText message

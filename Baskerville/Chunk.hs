{-# LANGUAGE TemplateHaskell #-}

module Baskerville.Chunk where

import Codec.Compression.Zlib
import Control.Lens
import Data.Array
import Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Int
import qualified Data.IntMap as IM
import Data.Serialize
import Data.Word
import Debug.Trace

import Baskerville.Coords

type ChunkArray = Array BCoord Word8

newtype MicroChunk = MicroChunk { getMicroChunk :: ChunkArray }
    deriving (Eq, Show)

putMicroChunk :: Putter MicroChunk
putMicroChunk (MicroChunk a) = do
    putByteString . BS.pack $ elems a
    putByteString $ BS.replicate 2048 0x00 -- block meta
    putByteString $ BS.replicate 2048 0xff -- block light
    putByteString $ BS.replicate 2048 0xff -- sky light
    -- not sending additional array

newtype ChunkIx = ChunkIx { unChunkIx :: (Int32, Int32) }
    deriving (Eq, Ix, Ord, Show)

data Chunk = Chunk { _cIx :: ChunkIx, _cBlocks :: IM.IntMap MicroChunk }
    deriving (Eq, Show)

makeLenses ''Chunk

putChunk :: Putter Chunk
putChunk (Chunk (ChunkIx (x, z)) blocks) = let
    bitmap = foldr (.|.) 0 $ map (shiftL 1) $ IM.keys blocks
    chunks = runPut $ mapM_ putMicroChunk $ IM.elems blocks
    biomes = BS.replicate 256 0x00
    compressed = compress . LBS.fromChunks $ [chunks, biomes]
    len = fromIntegral $ LBS.length compressed
    in do
        put x
        put z
        put True -- whether biome data
        putWord16be bitmap
        putWord16be 0 -- additional bitmap
        putWord32be len
        putLazyByteString compressed

-- | Create a new array with a given value.
newFilledArray :: Word8 -> ChunkArray
newFilledArray x = listArray (BCoord 0 0 0, BCoord 15 15 15) $ repeat x

-- | Create a zeroed-out chunk array.
newArray :: ChunkArray
newArray = newFilledArray 0x0

-- | Create a chunk at the given chunk coordinates.
newChunk :: ChunkIx -> Chunk
newChunk i = Chunk i IM.empty

type Generator = Int -> Maybe MicroChunk -> Maybe MicroChunk

pureGenerator :: (Int -> MicroChunk -> MicroChunk) -> Generator
pureGenerator g i = fmap $ g i

-- | Run a generator on a given chunk.
runGenerator :: Generator -> Chunk -> Chunk
runGenerator g chunk = let
    f :: Int -> Chunk -> Chunk
    f i = cBlocks %~ IM.alter (g i) i
    in foldr f chunk [0..15]

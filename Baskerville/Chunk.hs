module Baskerville.Chunk where

import Data.Array
import Data.Word

import Baskerville.Coords

type ChunkArray = Array BCoord Word8

data Chunk = Chunk { cX :: Int, cZ :: Int, cBlocks :: ChunkArray }
    deriving (Show)

-- | Create a zeroed-out chunk array.
newArray :: ChunkArray
newArray = let boundaries = (BCoord 0 0 0, BCoord 16 16 128)
    in array boundaries [(i, 0) | i <- range boundaries]

-- | Create a chunk at the given chunk coordinates.
newChunk :: (Int, Int) -> Chunk
newChunk (x, z) = Chunk x z newArray

runGenerator :: (ChunkArray -> ChunkArray) -> Chunk -> Chunk
runGenerator f c = c { cBlocks = f $ cBlocks c }

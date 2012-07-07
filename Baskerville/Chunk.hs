{-# LANGUAGE TemplateHaskell #-}

module Baskerville.Chunk where

import Data.Array
import Data.Int
import Data.Lens.Common
import Data.Lens.Template
import Data.Serialize
import Data.Word

import Baskerville.Coords

type ChunkArray = Array BCoord Word8

data Chunk = Chunk { _cX, _cZ :: Int32, _cBlocks :: ChunkArray }
    deriving (Eq, Show)

$( makeLens ''Chunk )

instance Serialize Chunk where
    put (Chunk x z blocks) = do
        put x
        put z
        put blocks

    get = do
        x <- get
        z <- get
        blocks <- get
        return $ Chunk x z blocks

-- | Create a zeroed-out chunk array.
newArray :: ChunkArray
newArray = let boundaries = (BCoord 0 0 0, BCoord 16 16 128)
    in array boundaries [(i, 0) | i <- range boundaries]

-- | Create a chunk at the given chunk coordinates.
newChunk :: Int32 -> Int32 -> Chunk
newChunk x z = Chunk x z newArray

runGenerator :: (ChunkArray -> ChunkArray) -> Chunk -> Chunk
runGenerator = modL cBlocks

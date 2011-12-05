module Baskerville.Chunk.Generators where

import Data.Array.ST

import Baskerville.Chunk
import Baskerville.Coords

-- | Ensure that the chunk has safety features enabled.
safety :: ChunkArray -> ChunkArray
safety ca = runSTArray $ do
    a <- thaw ca
    let zipper y val = zipWith3 writeArray (repeat a) (range (BCoord 0 0 y, BCoord 15 15 y)) (repeat val)
    sequence_ $ zipper 0 1
    sequence_ $ zipper 126 0
    sequence_ $ zipper 127 0
    return a

-- | Put some boring things into the chunk.
boring :: ChunkArray -> ChunkArray
boring ca = runSTArray $ do
    a <- thaw ca
    sequence_ $ zipWith3 writeArray (repeat a) (range (BCoord 0 0 0, BCoord 15 15 63)) (repeat 2)
    return a

module Baskerville.Chunk.Generators where

import Data.Array.ST

import Baskerville.Chunk
import Baskerville.Coords

-- | Write a plane of data all at once.
plane :: (MArray a e m) => a BCoord e -> Int -> e -> m ()
plane array y value = let
    as = repeat array
    vs = repeat value
    ixs = range (BCoord 0 0 y, BCoord 15 15 y)
    in sequence_ $ zipWith3 writeArray as ixs vs

-- | Ensure that the chunk has safety features enabled.
safety :: ChunkArray -> ChunkArray
safety ca = runSTArray $ do
    a <- thaw ca
    plane a 0 1
    plane a 126 0
    plane a 127 0
    return a

-- | Put some boring things into the chunk.
boring :: ChunkArray -> ChunkArray
boring ca = runSTArray $ do
    a <- thaw ca
    sequence_ $ zipWith3 writeArray (repeat a) (range (BCoord 0 0 0, BCoord 15 15 63)) (repeat 2)
    return a

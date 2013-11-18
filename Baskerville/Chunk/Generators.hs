module Baskerville.Chunk.Generators where

import Data.Array.ST
import Data.Int

import Baskerville.Chunk
import Baskerville.Coords

-- | Write a plane of data all at once.
plane :: (MArray a e m) => a BCoord e -> Word8 -> e -> m ()
plane array y value = let
    as = repeat array
    vs = repeat value
    ixs = range (BCoord 0 0 y, BCoord 15 15 y)
    in sequence_ $ zipWith3 writeArray as ixs vs

-- | Ensure that the chunk has safety features enabled.
safety :: Generator
safety i mmc = case mmc of
    Nothing -> Nothing
    Just (MicroChunk ca) -> case i of
        0x0 -> Just . MicroChunk $ runSTArray $ do
            a <- thaw ca
            plane a 0x0 0x1
            return a
        0xf -> Just . MicroChunk $ runSTArray $ do
            a <- thaw ca
            plane a 0xe 0x0
            plane a 0xf 0x0
            return a

-- | Put some boring things into the chunk.
boring :: Generator
boring i _
    | i < 8 = Just . MicroChunk $ newFilledArray 0x2
    | otherwise = Nothing

module Baskerville.Utilities.Bits where

import Data.Bits

unpackNibbles :: [Int] -> [Int]
unpackNibbles (x:xs) = x .&. 0xf : x `shiftR` 4 : unpackNibbles xs
unpackNibbles [] = []

packNibbles :: [Int] -> [Int]
packNibbles (x1:x2:xs) = (x1 .|. (x2 `shiftL` 4)) : packNibbles xs
packNibbles (x:xs) = x:xs
packNibbles [] = []

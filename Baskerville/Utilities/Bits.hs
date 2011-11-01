module Baskerville.Utilities.Bits where

import Data.Bits

unpackNibbles (x:xs) = x .&. 0xf : x `shiftR` 4 : unpackNibbles xs
unpackNibbles [] = []

packNibbles (x1:x2:xs) = (x1 .|. (x2 `shiftL` 4)) : packNibbles xs
packNibbles (x:xs) = x:xs
packNibbles [] = []

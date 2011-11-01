module Baskerville.Utilities.Chat where

import Data.Char

hashStringIndex s = foldr ((+) . ord) 0 s `mod` 16

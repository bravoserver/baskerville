import Char
import Data.Bits
import List
import Test.QuickCheck
import Text.Printf

import Baskerville.Utilities.Bits

-- From some tutorial somewhere.
mapper (d, t) = printf "%-30s: " d >> t
main = mapM_ mapper tests

-- Baskerville.Utilities.Bits
unpackNibblesMod2 :: [Int] -> Bool
unpackNibblesMod2 x = length (unpackNibbles x) `mod` 2 == 0
packUnpackNibbles :: [Int] -> Bool
packUnpackNibbles x = packNibbles (unpackNibbles x) == x

tests = [("unpackNibbles/mod2", quickCheck unpackNibblesMod2)
        ,("packNibbles.unpackNibbles/id", quickCheck packUnpackNibbles)]

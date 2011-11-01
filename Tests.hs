import Char
import Data.Bits
import List
import Test.QuickCheck
import Text.Printf

import Baskerville.Utilities.Bits
import Baskerville.Utilities.Chat

-- From some tutorial somewhere.
mapper (d, t) = printf "%-30s: " d >> t
main = mapM_ mapper tests

-- Baskerville.Utilities.Bits
unpackNibblesMod2 x = length (unpackNibbles x) `mod` 2 == 0
packUnpackNibbles x = packNibbles (unpackNibbles x) == x

-- Baskerville.Utilities.Chat
hashStringIndexRange x = let i = hashStringIndex x in i >= 0 && i < 16

tests = [("unpackNibbles/mod2", quickCheck unpackNibblesMod2)
        ,("packNibbles.unpackNibbles/id", quickCheck packUnpackNibbles)
        ,("hashStringIndex/range", quickCheck hashStringIndexRange)]

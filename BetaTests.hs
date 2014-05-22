-- Copyright (C) 2014 Google Inc. All rights reserved.
--
-- Licensed under the Apache License, Version 2.0 (the "License"); you may not
-- use this file except in compliance with the License. You may obtain a copy
-- of the License at
--
-- http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
-- WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
-- License for the specific language governing permissions and limitations
-- under the License.
import Char
import Control.Monad
import Data.Attoparsec.ByteString
import Data.Bits
import qualified Data.ByteString as BS
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Word as W
import List
import Test.QuickCheck
import Text.Printf

import Baskerville.Beta.Packets

-- From some tutorial somewhere.
mapper (d, t) = printf "%-30s: " d >> t
main = mapM_ mapper tests

instance Arbitrary BS.ByteString where
    arbitrary = liftM BS.pack (arbitrary :: Gen [W.Word8])
    shrink = map BS.pack . (shrink :: [W.Word8] -> [[W.Word8]]) . BS.unpack

instance Arbitrary T.Text where
    arbitrary = liftM T.pack (arbitrary :: Gen String)
    shrink = map T.pack . (shrink :: String -> [String]) . T.unpack

-- | Pull a parser into Maybe.
maybeParse :: Parser a -> BS.ByteString -> Maybe a
maybeParse p bs = maybeResult $ parse p bs

-- | Pull a parser into Maybe and use Maybe to determine whether the parse
--   succeeded.
parseBuild :: Parser a -> (a -> BS.ByteString) -> BS.ByteString -> Bool
parseBuild parser builder bs = case
    maybeParse parser bs >>= Just . builder of
        Just result -> bs == result
        Nothing -> True

buildParse :: Eq a => (a -> BS.ByteString) -> Parser a -> a -> Bool
buildParse builder parser x = case
    maybeParse parser $ builder x of
        Just result -> x == result
        Nothing -> False

-- Baskerville.Beta.Packets
buildParseWord16 = buildParse bWord16 pWord16
buildParseWord32 = buildParse bWord32 pWord32
buildParseWord64 = buildParse bWord64 pWord64
buildParseUcs2 = buildParse bUcs2 pUcs2

tests = [("bWord16.pWord16/id", quickCheck buildParseWord16)
        ,("bWord32.pWord32/id", quickCheck buildParseWord32)
        ,("bWord64.pWord64/id", quickCheck buildParseWord64)
        ,("bUcs2.pUcs2/id", quickCheck buildParseUcs2)]

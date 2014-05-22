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
import Data.Bits
import Data.Char
import Data.List
import Test.QuickCheck
import Text.Printf

import Baskerville.Utilities.Bits
import Baskerville.Utilities.Chat

-- From some tutorial somewhere.
mapper :: PrintfArg t => (t, IO b) -> IO b
mapper (d, t) = printf "%-30s: " d >> t
main = mapM_ mapper tests

-- Baskerville.Utilities.Bits
unpackNibblesMod2 x = length (unpackNibbles x) `mod` 2 == 0
packUnpackNibbles x = packNibbles (unpackNibbles x) == x

-- Baskerville.Utilities.Chat
hashStringIndexRange x = let i = hashStringIndex x in i >= 0 && i < 15
betaChatNameHead x = head (betaChatName x) == '§'
betaChatNameSuffix x = isSuffixOf "§f" (betaChatName x)
consoleChatNamePrefix x = isPrefixOf "\x1b[" (consoleChatName x)
consoleChatNameSuffix x = isSuffixOf "\x1b[0m" (consoleChatName x)

tests = [("unpackNibbles/mod2", quickCheck unpackNibblesMod2)
        ,("packNibbles.unpackNibbles/id", quickCheck packUnpackNibbles)
        ,("hashStringIndex/range", quickCheck hashStringIndexRange)
        ,("betaChatName/head", quickCheck betaChatNameHead)
        ,("betaChatName/suffix", quickCheck betaChatNameSuffix)
        ,("consoleChatName/prefix", quickCheck consoleChatNamePrefix)
        ,("consoleChatName/suffix", quickCheck consoleChatNameSuffix)]

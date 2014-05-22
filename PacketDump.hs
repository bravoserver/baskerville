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
import Baskerville.Beta.Packets
import qualified Data.ByteString as BS
import Data.Serialize
import Numeric
import System.IO

splitBytes str = let
    strs = words str
    in map (fst . head . readHex) strs

strToBs s = BS.pack $ map fromIntegral s

getPackets :: BS.ByteString -> Either String ([Packet], BS.ByteString)
getPackets bytes = case runGetPartial get bytes of
    Fail s -> Left s
    Partial _ -> Right ([], bytes)
    Done packet bytes' -> case getPackets bytes' of
        Right (ps, bytes'') -> Right (packet : ps, bytes'')
        l -> l

showPackets (Left s) = "Parse error: " ++ s
showPackets (Right (ps, rem)) = let
    srem = "Remainder: " ++ show rem
    sps = map show ps
    allLines = sps ++ [srem]
    in unlines allLines

derp = showPackets . getPackets . strToBs . splitBytes

main = interact derp

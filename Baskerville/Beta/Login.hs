{-# LANGUAGE OverloadedStrings #-}
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

module Baskerville.Beta.Login where

import Control.Applicative
import Control.Monad
import qualified Data.ByteString as BS
import Data.Serialize hiding (encode)
import qualified Data.Text as T

import Baskerville.Beta.Packets

data LoginPacket = LoginStart T.Text | LoginSuccess T.Text T.Text
    deriving (Eq, Show)

getLogin :: Get LoginPacket
getLogin = do
    -- Discard the packet length.
    void getInteger
    header <- getInteger
    case header of
        0x00 -> LoginStart <$> getText
        _    -> error $ "Won't get login header " ++ show header

putLogin :: Putter LoginPacket
putLogin (LoginSuccess uuid username) = let
    uuid' = runPut (putText uuid)
    username' = runPut (putText username)
    in do
    putInteger . toInteger $ BS.length uuid' + BS.length username' + 1
    putWord8 0x02
    putText uuid
    putText username
putLogin lp = error $ "Won't put " ++ show lp

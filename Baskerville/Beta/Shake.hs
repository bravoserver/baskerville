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

module Baskerville.Beta.Shake where

import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.Serialize hiding (encode)
import qualified Data.Text as T
import Data.Word

import Baskerville.Beta.Packets
import Baskerville.Beta.Server

data ShakeStyle = NewStatus | NewLogin
    deriving (Enum, Eq, Show)

instance Serialize ShakeStyle where
    put NewStatus = putWord8 0x01
    put NewLogin  = putWord8 0x02
    get = do
        m <- getWord8
        return $ case m of
            0x01 -> NewStatus
            0x02 -> NewLogin
            _    -> error $ "Insufficient style in shake: " ++ show m

data Handshake = Handshake Integer T.Text Word16 ShakeStyle
    deriving (Eq, Show)

getHandshake :: Get Handshake
getHandshake = do
    -- Discard the packet length.
    void getInteger
    -- And the header too.
    void getInteger
    protocol <- getInteger
    host     <- getText
    port     <- getWord16be
    style    <- get
    return $! Handshake protocol host port style

data StatusPacket = StatusRequest
                  | StatusResponse ServerInfo
                  | StatusPing Word64
    deriving (Eq, Show)

getStatus :: Get StatusPacket
getStatus = do
    -- Discard the packet length
    void getInteger
    header <- getInteger
    case header of
        0x00 -> return StatusRequest
        0x01 -> StatusPing <$!> getWord64be
        _    -> error $ "Won't get status header " ++ show header

putStatus :: Putter StatusPacket
putStatus (StatusResponse info) = putPacketHeader 0x00 $ let
    info' = encode info
    infoLength = toInteger $ BSL.length info'
    in do
    -- Length and data
    putInteger infoLength
    putLazyByteString info'
putStatus (StatusPing time) = putPacketHeader 0x01 $ putWord64be time
putStatus sp = error $ "Won't put " ++ show sp

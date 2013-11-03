{-# LANGUAGE OverloadedStrings #-}

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
putStatus (StatusResponse info) = let
    info' = encode info
    infoLength = toInteger $ BSL.length info'
    in do
    putInteger $ infoLength + 3
    -- Packet header
    putWord8 0x00
    -- Length and data
    putInteger infoLength
    putLazyByteString info'
putStatus (StatusPing time) = do
    -- Fixed length
    putWord8 0x05
    -- Packet header
    putWord8 0x01
    putWord64be time
putStatus sp = error $ "Won't put " ++ show sp

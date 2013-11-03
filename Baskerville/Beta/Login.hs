{-# LANGUAGE OverloadedStrings #-}

module Baskerville.Beta.Login where

import Control.Applicative
import Control.Monad
import Data.Aeson
import qualified Data.ByteString as BS
import Data.Serialize hiding (encode)
import qualified Data.Text as T
import Data.Word

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

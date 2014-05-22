{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
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

module Baskerville.Beta.Server where

import Control.Lens hiding ((.=))
import Data.Aeson
import qualified Data.Map as M

import Baskerville.Chunk

data ServerVersion = Version
    deriving (Eq, Show)

instance ToJSON ServerVersion where
    toJSON Version = object [ "name" .= ("1.7.2" :: String)
                            , "protocol" .= (4 :: Int) ]

data ServerInfo = Info ServerVersion
    deriving (Eq, Show)

instance ToJSON ServerInfo where
    toJSON (Info version) = let
        players = object [ "max"    .= (1000 :: Int)
                         , "online" .= (0 :: Int) ]
        description = object [ "text" .= ("Baskerville" :: String) ]
        in object [ "version"     .= version
                  , "players"     .= players
                  , "description" .= description ]

data Server = Server { _sInfo :: ServerInfo
                     , _sWorld :: M.Map ChunkIx Chunk }
    deriving (Eq, Show)

makeLenses ''Server

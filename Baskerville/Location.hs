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

module Baskerville.Location where

import Control.Applicative
import Control.Lens.TH
import Data.Serialize

data Position = Position { _px, _py, _pstance, _pz :: Double }
    deriving (Eq, Show)

instance Serialize Position where
    put (Position x y stance z) = do
        putFloat64be x
        putFloat64be y
        putFloat64be stance
        putFloat64be z
    get = do
        x <- getFloat64be
        y <- getFloat64be
        stance <- getFloat64be
        z <- getFloat64be
        return $ Position x y stance z

makeLenses ''Position

data Orientation = Orientation { _oyaw, _opitch :: Float }
    deriving (Eq, Show)

instance Serialize Orientation where
    put (Orientation yaw pitch) = putFloat32be yaw >> putFloat32be pitch
    get = Orientation <$> getFloat32be <*> getFloat32be

makeLenses ''Orientation

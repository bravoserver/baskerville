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

module Baskerville.Coords where

import Control.Lens.TH
import Data.Int
import Data.Ix
import Data.Serialize
import Data.Word

-- | Block coordinates.
data BCoord = BCoord { _bcy :: Word8, _bcz, _bcx :: Int32 }
    deriving (Eq, Ix, Ord, Show)

makeLenses ''BCoord

instance Serialize BCoord where
    put (BCoord y z x) = put x >> put y >> put z
    get = do
        x <- get
        y <- get
        z <- get
        return $ BCoord y z x

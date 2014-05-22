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
module Baskerville.Utilities.Geometry where

data Coord = Coord {cx :: Float, cy :: Float, cz :: Float}
    deriving (Show, Eq, Ord)

coord2tuple :: Coord -> (Float, Float, Float)
coord2tuple (Coord x y z) = (x, y, z)

tuple2coord :: (Float, Float, Float) -> Coord
tuple2coord (x, y, z) = Coord x y z

mapCoord :: (Float -> Float) -> Coord -> Coord
mapCoord f (Coord x y z) = Coord (f x) (f y) (f z)

absCoord :: Coord -> Coord
absCoord = mapCoord abs

maxCoord :: Coord -> Float
maxCoord (Coord x y z) = maximum [x, y, z]

normalizeCoord :: Coord -> Coord
normalizeCoord c@(Coord x y z) = let m = maxCoord (absCoord c)
    in Coord (x / m) (y / m) (z / m)

diffCoords :: Coord -> Coord -> Coord
diffCoords (Coord x1 y1 z1) (Coord x2 y2 z2) =
    Coord (x1 - x2) (y1 - y2) (z1 - z2)

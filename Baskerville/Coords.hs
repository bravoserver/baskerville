module Baskerville.Coords where

import Data.Ix

data BCoord = BCoord { bcx :: Int, bcz :: Int, bcy :: Int }
    deriving (Eq, Ix, Ord, Show)

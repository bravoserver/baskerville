{-# LANGUAGE TemplateHaskell #-}

module Baskerville.Coords where

import Data.Int
import Data.Ix
import Data.Lens.Template
import Data.Serialize

-- | Block coordinates.
data BCoord = BCoord { _bcx, _bcz, _bcy :: Int32 }
    deriving (Eq, Ix, Ord, Show)

instance Serialize BCoord where
    put (BCoord x z y) = put x >> put y >> put z
    get = do
        x <- get
        y <- get
        z <- get
        return $ BCoord x z y

$( makeLens ''BCoord )

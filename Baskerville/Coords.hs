{-# LANGUAGE TemplateHaskell #-}

module Baskerville.Coords where

import Control.Lens.TH
import Data.Int
import Data.Ix
import Data.Serialize
import Data.Word

-- | Block coordinates.
data BCoord = BCoord { _bcx, _bcz :: Int32, _bcy :: Word8 }
    deriving (Eq, Ix, Ord, Show)

makeLenses ''BCoord

instance Serialize BCoord where
    put (BCoord x z y) = put x >> put y >> put z
    get = do
        x <- get
        y <- get
        z <- get
        return $ BCoord x z y

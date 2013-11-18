{-# LANGUAGE TemplateHaskell #-}

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

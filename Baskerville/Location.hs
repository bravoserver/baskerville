{-# LANGUAGE TemplateHaskell #-}

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

makeLenses Position

data Orientation = Orientation { _oyaw, _opitch :: Float }
    deriving (Eq, Show)

instance Serialize Orientation where
    put (Orientation yaw pitch) = putFloat32be yaw >> putFloat32be pitch
    get = Orientation <$> getFloat32be <*> getFloat32be

makeLenses Orientation

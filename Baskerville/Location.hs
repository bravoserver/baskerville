{-# LANGUAGE TemplateHaskell #-}

module Baskerville.Location where

import Data.Lens.Template
import Data.Serialize

data Position = Position { _px, _py, _pstance, _pz :: Double }
    deriving (Eq, Show)

instance Serialize Position where
    put (Position x y stance z) = do
        put x
        put y
        put stance
        put z
    get = do
        x <- get
        y <- get
        stance <- get
        z <- get
        return $ Position x y stance z

data Orientation = Orientation { _oyaw, _opitch :: Float }
    deriving (Eq, Show)

instance Serialize Orientation where
    put (Orientation yaw pitch) = put yaw >> put pitch
    get = do
        yaw <- get
        pitch <- get
        return $ Orientation yaw pitch

$( makeLenses [''Position, ''Orientation] )

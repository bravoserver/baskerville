{-# LANGUAGE TemplateHaskell #-}

module Baskerville.Coords where

import Data.Ix
import Data.Lens.Template

data BCoord = BCoord { _bcx :: Int, _bcz :: Int, _bcy :: Int }
    deriving (Eq, Ix, Ord, Show)

$( makeLens ''BCoord )

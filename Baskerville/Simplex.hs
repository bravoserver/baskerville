module Baskerville.Simplex where

import Control.Monad.Random
import Data.List
import System.Random.Shuffle

-- | The edges of a two-dimensional simplex field.
edges2 :: [[Integer]]
edges2 = sort $ nub $ concat [
    permutations [0, 1, 1], permutations [0, 1, -1], permutations [0, -1, -1]]

-- | Dot product for lists of numbers.
dot :: Num a => [a] -> [a] -> a
dot u v = sum $ zipWith (*) u v

-- | The constant 'f' for two-dimensional fields.
f2 :: Double
f2 = (sqrt 3 - 1) / 2

-- | The constant 'g' for two-dimensional fields.
g2 :: Double
g2 = (3 - sqrt 3) / 6

-- | The size of a field.
size :: Integer
size = 1024

field :: (MonadRandom m) => m [Integer]
field = shuffleM [0 .. (size - 1)]

-- | Another thing!
t :: (Fractional a) => a -> a -> a
t x y = 0.5 - (x^2) - (y^2)

-- simplex2 :: Num a, Num b, Floating c => a -> b -> b -> c

module Baskerville.Simplex where

import Control.Monad.Random
import Data.List
import System.Random.Shuffle

-- | The edges of a two-dimensional simplex field.
edges2 :: [[Int]]
edges2 = sort $ nub $ concat [
    permutations [0, 1, 1], permutations [0, 1, -1], permutations [0, -1, -1]]

edge2 :: Int -> [Int]
edge2 i = edges2 !! (i `mod` length edges2)

-- | Dot product for lists of numbers.
dot :: (Num a) => [a] -> [a] -> a
dot u v = sum $ zipWith (*) u v

-- | The constant 'f' for two-dimensional fields.
f2 :: Double
f2 = (sqrt 3 - 1) / 2

-- | The constant 'g' for two-dimensional fields.
g2 :: Double
g2 = (3 - sqrt 3) / 6

-- | The size of a field.
size :: Int
size = 1024

field :: (MonadRandom m) => m [Int]
field = shuffleM [0 .. (size - 1)]

(!!!) :: [a] -> Int -> a
xs !!! i = xs !! (i `mod` size)

-- | Squish 2D coords onto a simplex grid.
--   Returns a tuple of the ints for array lookups, and the fractional parts
--   for inter-box interpolation. You could think of it as (I, J, X, Y).
squish2 :: (Integral a) => Double -> Double -> (a, a, Double, Double)
squish2 x y = let
    s = (x + y) * f2
    i = floor $ x + s
    j = floor $ y + s
    t = fromIntegral (i + j) * g2
    in (i, j, x + t - fromIntegral i, y + t - fromIntegral j)

coords2 :: Double -> Double -> [(Double, Double)]
coords2 x y = if x > y
    then [(x, y), (x - 1 + g2, y + g2), (x - 1 + g2 * 2, y - 1 + g2 * 2)]
    else [(x, y), (x + g2, y - 1 + g2), (x - 1 + g2 * 2, y - 1 + g2 * 2)]

-- | Interpolator for 2D coords.
t2 :: (Fractional a) => a -> a -> a
t2 x y = 0.5 - (x^2 + y^2)

n2 :: (Fractional a, Ord a) => (a, a) -> Int -> a
n2 (x, y) g = let
    t = t2 x y
    edge = map fromIntegral $ edge2 g
    in if t > 0 then (t^4) * dot [x, y] edge else 0

simplex2 :: (MonadRandom m) => Double -> Double -> m Double
simplex2 sx sy = let
    (i, j, dx, dy) = squish2 sx sy
    coords = coords2 dx dy
    in do
        p <- field
        let g1 = p !!! (i + p !!! j)
        let g2 = if dx > dy
            then p !!! (i + 1 + p !!! j)
            else p !!! (i + p !!! (j + 1))
        let g3 = p !!! (i + 1 + p !!! (j + 1))
        let gradients = [g1, g2, g3]
        let n = sum $ zipWith n2 coords gradients
        return $ n * 70

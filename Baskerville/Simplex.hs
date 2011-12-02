module Baskerville.Simplex where

import Data.List

edges2 :: [[Integer]]
edges2 = sort $ nub $ concat [
    permutations [0, 1, 1], permutations [0, 1, -1], permutations [0, -1, -1]]

dot :: Num a => [a] -> [a] -> a
dot u v = sum $ zipWith (*) u v

f2 :: Double
f2 = (sqrt 3 - 1) / 2
g2 :: Double
g2 = (3 - sqrt 3) / 6

size = 1024
p i = [0..1023] !! mod i 1024
t x y = 0.5 - (x^2) - (y^2)

-- simplex2 :: Num a, Num b, Floating c => a -> b -> b -> c

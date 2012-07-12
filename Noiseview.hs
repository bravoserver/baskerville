import Baskerville.Simplex
import Control.Monad
import Control.Monad.Random
import System.Environment
import System.Random

main = let
    gen = mkStdGen 0
    in do
    putStrLn "P1"
    putStrLn "256 256"
    forM [0..255] $ \x -> do
        forM [0..255] $ \y -> do
            let pix = evalRand (simplex2 x y) gen
            let adjusted = floor $ (pix + 1) * 127.5
            putStr $ show adjusted ++ " "
        putStrLn " "

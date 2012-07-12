import Baskerville.Simplex
import Control.Monad
import Control.Monad.Random
import System.Environment
import System.Random

main = let
    gen = mkStdGen 0
    in do
    putStrLn "P2"
    putStrLn "512 512"
    forM [0..511] $ \x -> do
        forM [0..511] $ \y -> do
            let pix = evalRand (simplex2 (x/512) (y/512)) gen
            let adjusted = floor $ (pix + 1) * 127.5
            putStr $ show adjusted ++ " "
        putStrLn " "

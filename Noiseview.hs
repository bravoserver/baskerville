import Baskerville.Simplex
import Control.Monad
import Control.Monad.Random
import System.Environment
import System.Random

main = do
    putStrLn "P2"
    putStrLn "512 512"
    putStrLn "255"
    forM [0..511] $ \x -> do
        forM [0..511] $ \y -> do
            let pix = simplex2 0 (x/512) (y/512)
            let adjusted = floor $ (pix + 1) * 127.5
            putStr $ show adjusted ++ " "
        putStrLn " "

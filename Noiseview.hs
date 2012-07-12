import Baskerville.Simplex
import Control.Monad
import Control.Monad.Random
import System.Environment
import System.Random

readDouble x = read x :: Double

dim = 800

main = do
    ssx : ssy : _ <- getArgs
    let sx = readDouble ssx
    let sy = readDouble ssy
    putStrLn "P2"
    putStrLn $ show dim ++ " " ++ show dim
    putStrLn "255"
    forM [0..(dim - 1)] $ \x -> do
        forM [0..(dim - 1)] $ \y -> do
            let pix = octaves2 5 0 (x * sx / dim) (y * sy / dim)
            let adjusted = floor $ (pix + 1) * 127.5
            putStr $ show adjusted ++ " "
        putStrLn " "

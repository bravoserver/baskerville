import Baskerville.Simplex
import Control.Monad
import Control.Monad.Random
import System.Environment
import System.Random

readDouble x = read x :: Double

main = do
    ssx : ssy : _ <- getArgs
    let sx = readDouble ssx
    let sy = readDouble ssy
    putStrLn "P2"
    putStrLn "512 512"
    putStrLn "255"
    forM [0..511] $ \x -> do
        forM [0..511] $ \y -> do
            let pix = octaves2 5 0 (x * sx / 512) (y * sy / 512)
            let adjusted = floor $ (pix + 1) * 127.5
            putStr $ show adjusted ++ " "
        putStrLn " "

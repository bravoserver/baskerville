-- Copyright (C) 2014 Google Inc. All rights reserved.
--
-- Licensed under the Apache License, Version 2.0 (the "License"); you may not
-- use this file except in compliance with the License. You may obtain a copy
-- of the License at
--
-- http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
-- WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
-- License for the specific language governing permissions and limitations
-- under the License.
import Baskerville.Simplex
import Control.Monad
import Control.Monad.Random
import Data.IORef
import System.Environment
import System.IO
import Text.Printf

readDouble x = read x :: Double

dim = 800 :: Int

progress x total = let
    l = concat $ iterate (map (10 *)) [1, 2, 4, 5, 8]
    whether = x == head (dropWhile (< x) l)
    percent = fromIntegral x * 100.0 / fromIntegral total :: Double
    s = "Status: " ++ show x ++ "/" ++ show total ++ printf " (%.2f%%)" percent
    in when whether $ putStrLn s

main = let
    total = dim * dim
    in do
        ssx : ssy : _ <- getArgs
        let sx = readDouble ssx
        let sy = readDouble ssy
        counter <- newIORef 0
        handle <- openFile "noise.pnm" WriteMode
        hPutStrLn handle "P2"
        hPutStrLn handle $ show dim ++ " " ++ show dim
        hPutStrLn handle "255"
        forM_ [0..(dim - 1)] $ \x -> do
            let fx = fromIntegral x
            forM_ [0..(dim - 1)] $ \y -> do
                let fy = fromIntegral y
                let fdim = fromIntegral dim
                let pix = octaves2 5 0 (fx * sx / fdim) (fy * sy / fdim)
                let adjusted = floor $ (pix + 1) * 127.5
                hPutStr handle $ show adjusted ++ " "
                count <- readIORef counter
                progress count total
                writeIORef counter $ count + 1
            hPutStrLn handle " "
        hClose handle

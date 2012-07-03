module Baskerville.Utilities.Chat where

import Data.Char

hashStringIndex :: [Char] -> Int
hashStringIndex s = foldr ((+) . ord) 0 s `mod` 15

betaColor :: Int -> [Char]
betaColor i = ['§', "0123456789abcde" !! i]

consoleColor :: Int -> [Char]
consoleColor i =
    "\x1b[" ++ (["1;30", "34", "32", "36", "31", "35", "33", "1;37", "37",
    "1;34", "1;32", "1;36", "1;31", "1;35", "1;33"] !! i) ++ "m"

betaChatName :: [Char] -> [Char]
betaChatName s = betaColor (hashStringIndex s) ++ s ++ "§f"

consoleChatName :: [Char] -> [Char]
consoleChatName s = consoleColor (hashStringIndex s) ++ s ++ "\x1b[0m"

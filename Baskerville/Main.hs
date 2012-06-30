module Main where

import Network
import Data.Conduit
import Data.Conduit.Network

-- import Baskerville.Beta.Protocol

app :: Application IO
app source sink = do
    putStrLn "Before app..."
    source $$ sink
    putStrLn "After app!"

startServer :: IO ()
startServer = runTCPServer (ServerSettings 12321 HostAny) app

main :: IO ()
main = withSocketsDo $ do
    putStrLn "Starting up..."
    startServer

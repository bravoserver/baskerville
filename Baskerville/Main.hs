module Main where

import Control.Monad.IO.Class
import Control.Monad.Trans.State (evalStateT)
import Data.Conduit
import Data.Conduit.Network
import Network

import Baskerville.Beta.Conduits
import Baskerville.Beta.Session

app :: Application (Session IO)
app appdata = do
    liftIO $ putStrLn "Before app..."
    let pSource = appSource appdata $= bsToPackets
    let pSink = packetsToBs =$ appSink appdata
    pSource $$ protocol =$ pSink
    liftIO $ putStrLn "After app!"

startServer :: (Session IO) ()
startServer = runTCPServer (serverSettings 25565 HostAny) app

main :: IO ()
main = withSocketsDo $ do
    putStrLn "Starting up..."
    s <- startingState
    evalStateT startServer s

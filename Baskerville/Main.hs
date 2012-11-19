{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Control.Distributed.Process
import Control.Distributed.Process.Node
import Control.Exception
import Control.Monad
import Control.Monad.Trans.State (evalStateT)
import Data.Binary
import Data.Conduit
import Data.Conduit.Network
import Data.Typeable
import Network
import Network.Transport.TCP

import Baskerville.Beta.Conduits
import Baskerville.Beta.Session

app :: Application (Session IO)
app appdata = do
    liftIO $ putStrLn "Before app..."
    let pSource = appSource appdata $= bsToPackets
        pSink = packetsToBs =$ appSink appdata
    pSource $$ protocol =$ pSink
    liftIO $ putStrLn "After app!"

startServer :: (Session IO) ()
startServer = runTCPServer (serverSettings 25565 HostAny) app

data NewSession = NewSession
    deriving (Typeable, Show)

instance Binary NewSession where
    put NewSession = putWord8 0
    get = getWord8 >> return NewSession

slProcess :: ReceivePort NewSession -> Process ()
slProcess rp = do
    message <- receiveChan rp
    case message of
        NewSession -> say "I was asked to create a new session!"
    slProcess rp

startSessionList :: Process (SendPort NewSession)
startSessionList = spawnChannelLocal slProcess

main :: IO ()
main = withSocketsDo $ do
    putStrLn "Starting TCP control socket..."
    etrans <- createTransport "localhost" "omniorb" defaultTCPParameters
    transport <- case etrans of
        Left exception -> throw exception
        Right t -> return t
    putStrLn "Creating local node..."
    node <- newLocalNode transport initRemoteTable
    putStrLn "Spinning up SessionList..."
    pid <- forkProcess node (void startSessionList)
    putStrLn $ "Forked, PID " ++ show pid
    putStrLn "Starting up..."
    s <- startingState
    evalStateT startServer s

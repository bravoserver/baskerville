module Main where

import Control.Monad.IO.Class
import Control.Monad.Trans.State (evalStateT)
import qualified Data.ByteString as BS
import Data.Conduit
import Data.Conduit.Cereal
import Data.Conduit.Network
import Data.Serialize
import Network

import Baskerville.Beta.Packets
import Baskerville.Beta.Session

toPackets :: Conduit BS.ByteString (Session IO) Packet
toPackets = conduitGet get

fromPackets :: Conduit Packet (Session IO) BS.ByteString
fromPackets = conduitPut put

app :: Application (Session IO)
app source sink = do
    liftIO $ putStrLn "Before app..."
    let pSource = source $= toPackets
    let pSink = fromPackets =$ sink
    pSource $$ protocol =$ pSink
    liftIO $ putStrLn "After app!"

startServer :: (Session IO) ()
startServer = runTCPServer (ServerSettings 25565 HostAny) app

main :: IO ()
main = withSocketsDo $ do
    putStrLn "Starting up..."
    s <- startingState
    evalStateT startServer s

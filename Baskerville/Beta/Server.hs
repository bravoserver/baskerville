{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Baskerville.Beta.Server where

import Control.Lens hiding ((.=))
import Data.Aeson

data ServerVersion = Version
    deriving (Eq, Show)

instance ToJSON ServerVersion where
    toJSON Version = object [ "name" .= ("1.7.2" :: String)
                            , "protocol" .= (4 :: Int) ]

data ServerInfo = Info ServerVersion
    deriving (Eq, Show)

instance ToJSON ServerInfo where
    toJSON (Info version) = let
        players = object [ "max"    .= (1000 :: Int)
                         , "online" .= (0 :: Int) ]
        description = object [ "text" .= ("Baskerville" :: String) ]
        in object [ "version"     .= version
                  , "players"     .= players
                  , "description" .= description ]

data Server = Server { _sInfo :: ServerInfo }
    deriving (Show)

makeLenses ''Server

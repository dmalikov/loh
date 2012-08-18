{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Loh.Core.Types where

import Control.Applicative ((<*>), (<$>), empty)
import Data.Aeson
import Data.Function (on)

import qualified Network.Lastfm as LFM


-- TrackInfo

type Album = String
type Artist = String
type Sec = Int
type Track = String

data TrackInfo = TrackInfo
  { album      ∷ Album
  , artist     ∷ Artist
  , currentSec ∷ Sec
  , totalSec   ∷ Sec
  , track      ∷ Track
  } deriving (Read, Show)

instance Eq TrackInfo where
  α == β = ((==) `on` artist) α β &&
           ((==) `on` album) α β &&
           ((==) `on` track) α β

instance FromJSON TrackInfo where
  parseJSON (Object o) = TrackInfo <$>
    o .: "album" <*>
    o .: "artist" <*>
    o .: "currentSec" <*>
    o .: "totalSec" <*>
    o .: "track"
  parseJSON _ = empty

instance ToJSON TrackInfo where
  toJSON τ = object
    [ "album"      .= album τ
    , "artist"     .= artist τ
    , "currentSec" .= currentSec τ
    , "totalSec"   .= totalSec τ
    , "track"      .= track τ
    ]


-- Player

data Player = Player
  { name    ∷ PlayerName
  , getInfo ∷ IO (Maybe TrackInfo)
  }

data PlayerName = Mocp | Mpd
  deriving (Eq, Ord, Read, Show)


-- Config

type LFMConfig = (LFM.APIKey, LFM.SessionKey, LFM.Secret)

instance FromJSON LFM.APIKey where
  parseJSON (Object o) = LFM.APIKey <$> o .: "apiKey"
  parseJSON _ = empty

instance ToJSON LFM.APIKey where
  toJSON (LFM.APIKey ak) = object [ "apiKey" .= ak ]

instance ToJSON LFM.SessionKey where
  toJSON (LFM.SessionKey sk) = object [ "session" .= object [ "key" .= sk ] ]

instance FromJSON LFM.Secret where
  parseJSON (Object o) = LFM.Secret <$> o .: "secret"
  parseJSON _ = empty

instance ToJSON LFM.Secret where
  toJSON (LFM.Secret s) = object [ "secret" .= s ]

instance FromJSON LFMConfig where
  parseJSON (Object o) = do
    ak ← o .: "apiKey"
    sk ← o .: "sessionKey"
    s  ← o .: "secret"
    return (LFM.APIKey ak, LFM.SessionKey sk, LFM.Secret s)
  parseJSON _ = empty

instance ToJSON LFMConfig where
  toJSON (LFM.APIKey ak, LFM.SessionKey sk, LFM.Secret s) = object
    [ "apiKey"     .= ak
    , "sessionKey" .= sk
    , "secret"     .= s
    ]


-- Port (?)

lohPort ∷ Num α ⇒ α
lohPort = 9114


-- Task

data TaskType = Scrobble | UpdateNowPlaying
  deriving (Read, Show)


instance FromJSON TaskType where
  parseJSON (Object o) = read <$> o .: "type"
  parseJSON _ = empty

instance ToJSON TaskType where
  toJSON t = object [ "type" .= show t ]

data Task = Task
  { typeT       ∷ TaskType
  , lfmConfigT  ∷ LFMConfig
  , trackInfoT  ∷ TrackInfo
  } deriving Show

instance FromJSON Task where
  parseJSON (Object o) = do
    t  ← read <$> o .: "type"
    l  ← o .: "lfmConfig"
    ti ← o .: "trackInfo"
    return $ Task t l ti
  parseJSON _ = empty

instance ToJSON Task where
  toJSON (Task t l ti) = object
    [ "type"       .= show t
    , "lfmConfig"  .= l
    , "trackInfo"  .= ti
    ]

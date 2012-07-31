{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Loh.Types where

import Control.Applicative ((<*>), (<$>))
import Data.Aeson
import Data.Function (on)
import Data.Time (UTCTime(..))

import qualified Data.ByteString.Char8 as BS
import qualified Network.Lastfm as LFM


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

data DBRecord = DBRecord
  { timestamp ∷ UTCTime
  , trackInfo ∷ TrackInfo
  } deriving (Read, Show)

data Player = Player
  { name ∷ PlayerName
  , getInfo ∷ IO (Maybe TrackInfo)
  }

data PlayerName = Mocp | Mpd
  deriving (Eq, Ord, Read, Show)

type LFMConfig = (LFM.APIKey, LFM.SessionKey, LFM.Secret)

instance FromJSON TrackInfo where
  parseJSON (Object o) = TrackInfo <$>
    o .: "album" <*>
    o .: "artist" <*>
    o .: "currentSec" <*>
    o .: "totalSec" <*>
    o .: "track"

instance ToJSON TrackInfo where
  toJSON τ = object
    [ "album"      .= album τ
    , "artist"     .= artist τ
    , "currentSec" .= currentSec τ
    , "totalSec"   .= totalSec τ
    , "track"      .= track τ
    ]

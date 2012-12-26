{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Loh.Core.Types where

import           Control.Applicative (empty, (<$>), (<*>))
import           Data.Aeson
import           Data.Function       (on)


-- TrackInfo

type Album = String
type Artist = String
type Sec = Int
type Title = String

data TrackInfo = TrackInfo
  { album      ∷ Album
  , artist     ∷ Artist
  , currentSec ∷ Sec
  , totalSec   ∷ Sec
  , title      ∷ Title
  } deriving (Read, Show)

instance Eq TrackInfo where
  α == β = ((==) `on` artist) α β &&
           ((==) `on` album) α β &&
           ((==) `on` title) α β

instance FromJSON TrackInfo where
  parseJSON (Object o) = TrackInfo <$>
    o .: "album" <*>
    o .: "artist" <*>
    o .: "currentSec" <*>
    o .: "totalSec" <*>
    o .: "title"
  parseJSON _ = empty

instance ToJSON TrackInfo where
  toJSON τ = object
    [ "album"      .= album τ
    , "artist"     .= artist τ
    , "currentSec" .= currentSec τ
    , "totalSec"   .= totalSec τ
    , "title"      .= title τ
    ]


-- Player

data Player = Player
  { name    ∷ PlayerName
  , getInfo ∷ IO (Maybe TrackInfo)
  }

data PlayerName = Mpd
  deriving (Eq, Ord, Read, Show)


-- Port (?)

lohPort ∷ Num α ⇒ α
lohPort = 9114

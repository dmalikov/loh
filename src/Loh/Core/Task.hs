{-# LANGUAGE OverloadedStrings #-}
module Loh.Core.Task where

import           Control.Applicative  (empty, (<$>), (<*>))
import           Data.Aeson

import           Loh.Core.LastFM.Auth
import           Loh.Core.Types

data Task = Task TaskType LFMConfig Track
  deriving (Show)

data TaskType = Scrobble | UpdateNowPlaying
  deriving (Eq, Read, Show)

data Track = Track (Maybe Album) Artist (Maybe Sec) Title
  deriving (Eq, Read, Show)

instance FromJSON Track where
  parseJSON (Object o) = Track <$>
    o .:? "album" <*>
    o .:  "artist" <*>
    o .:? "length" <*>
    o .:  "title"
  parseJSON _ = empty

instance ToJSON Track where
  toJSON (Track al ar s t) = object
    [ "album"  .= al
    , "artist" .= ar
    , "length" .= s
    , "title"  .= t
    ]

instance FromJSON TaskType where
  parseJSON (Object o) = read <$> o .: "type"
  parseJSON _ = empty

instance ToJSON TaskType where
  toJSON t = object [ "type" .= show t ]

instance FromJSON Task where
  parseJSON (Object o) = Task <$>
    read <$> o .: "type" <*>
    o .: "auth" <*>
    o .: "track"
  parseJSON _ = empty

instance ToJSON Task where
  toJSON (Task t l ti) = object
    [ "type"  .= show t
    , "auth"  .= l
    , "track" .= ti
    ]

-- | Convert TrackInfo to Track
toTrack ∷ TrackInfo → Track
toTrack (TrackInfo al ar _ l t) = Track (Just al) ar (Just l) t

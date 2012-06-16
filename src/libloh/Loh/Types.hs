{-# LANGUAGE TypeSynonymInstances #-}
module Loh.Types where

import Data.Function (on)

import qualified Data.Map as M
import qualified Network.Lastfm as LFM

newtype Timestamp = Timestamp Integer
  deriving (Read, Show)

type Album = String
type Artist = String
type Duration = Double
type Sec = Int
type Track = String

data TrackInfo = TrackInfo
  { album      ∷ Album
  , artist     ∷ Artist
  , currentSec ∷ Sec
  , duration   ∷ Duration
  , track      ∷ Track
  } deriving (Read, Show)

instance Eq TrackInfo where
  α == β = ((==) `on` artist) α β &&
           ((==) `on` album) α β &&
           ((==) `on` track) α β

data DBRecord = DBRecord Timestamp TrackInfo
  deriving (Read, Show)

data PlayerName = Mocp | Mpd
  deriving (Eq, Ord, Read, Show)

type PlayersInfo = M.Map PlayerName TrackInfo
type PlayersInfoToScrobble = M.Map PlayerName (TrackInfo, Maybe Duration)

type LFMConfig = (LFM.APIKey, LFM.SessionKey, LFM.Secret)

data ScrobbleStatus = ScrobbleDone | ScrobbleFailed

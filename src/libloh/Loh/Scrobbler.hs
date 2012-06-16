module Loh.Scrobbler
  ( nowPlaying, scrobbleTrack
  ) where

import Control.Applicative ((<$>))
import Data.Time (formatTime, getCurrentTime)
import System.Locale (defaultTimeLocale)

import qualified Network.Lastfm as LFM
import qualified Network.Lastfm.XML.Track as Track

import Loh.Types

toScrobbleStatus ∷ Either α β → ScrobbleStatus
toScrobbleStatus =
  either (const ScrobbleFailed) (const ScrobbleDone)

nowPlaying ∷ LFMConfig → TrackInfo → IO ScrobbleStatus
nowPlaying (ak, sk, s) ti =
  toScrobbleStatus <$> Track.updateNowPlaying
    (LFM.Artist $ artist ti)
    (LFM.Track $ track ti)
    (Just $ LFM.Album $ album ti)
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    ak sk s

scrobbleTrack ∷ LFMConfig → TrackInfo → IO ScrobbleStatus
scrobbleTrack (ak, sk, s) ti = do
  nts ← read . formatTime defaultTimeLocale "%s" <$> getCurrentTime
  toScrobbleStatus <$> Track.scrobble
    ( LFM.Timestamp nts
    , Just $ LFM.Album $ album ti
    , LFM.Artist $ artist ti
    , LFM.Track $ track ti
    , Nothing
    , Nothing
    , Nothing
    , Nothing
    , Nothing
    , Nothing
    , Nothing
    ) ak sk s

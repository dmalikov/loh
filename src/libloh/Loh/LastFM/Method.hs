module Loh.LastFM.Method
  ( loveTrack, nowPlaying, scrobbleTrack
  ) where

import Control.Applicative ((<$>))
import Data.Time (formatTime, getCurrentTime)
import System.Locale (defaultTimeLocale)

import Network.Lastfm (Lastfm, Response)
import qualified Network.Lastfm as LFM
import qualified Network.Lastfm.JSON.Track as Track

import Loh.Types


loveTrack ∷ LFMConfig → TrackInfo → Lastfm Response
loveTrack (ak, sk, s) ti =
  Track.love
    (LFM.Artist $ artist ti)
    (LFM.Track $ track ti)
    ak sk s

nowPlaying ∷ LFMConfig → TrackInfo →  Lastfm Response
nowPlaying (ak, sk, s) ti =
  Track.updateNowPlaying
    (LFM.Artist $ artist ti)
    (LFM.Track $ track ti)
    (Just $ LFM.Album $ album ti)
    Nothing
    Nothing
    Nothing
    Nothing
    (Just $ LFM.Duration $ totalSec ti)
    ak sk s

scrobbleTrack ∷ LFMConfig → TrackInfo → Lastfm Response
scrobbleTrack (ak, sk, s) ti = do
  nts ← read . formatTime defaultTimeLocale "%s" <$> getCurrentTime
  Track.scrobble
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

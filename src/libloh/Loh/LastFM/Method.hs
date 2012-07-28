module Loh.LastFM.Method
  ( loveTrack, nowPlaying, scrobbleTrack
  ) where

import Control.Applicative ((<$>))
import Data.Time (formatTime, getCurrentTime)
import System.Locale (defaultTimeLocale)

import qualified Network.Lastfm as LFM
import qualified Network.Lastfm.JSON.Track as Track

import Loh.Types


loveTrack ∷ LFMConfig → TrackInfo → IO LFMOperationStatus
loveTrack (ak, sk, s) ti =
  toLFMStatus <$> Track.love
    (LFM.Artist $ artist ti)
    (LFM.Track $ track ti)
    ak sk s

nowPlaying ∷ LFMConfig → TrackInfo → IO LFMOperationStatus
nowPlaying (ak, sk, s) ti =
  toLFMStatus <$> Track.updateNowPlaying
    (LFM.Artist $ artist ti)
    (LFM.Track $ track ti)
    (Just $ LFM.Album $ album ti)
    Nothing
    Nothing
    Nothing
    Nothing
    (Just $ LFM.Duration $ totalSec ti)
    ak sk s

scrobbleTrack ∷ LFMConfig → TrackInfo → IO LFMOperationStatus
scrobbleTrack (ak, sk, s) ti = do
  nts ← read . formatTime defaultTimeLocale "%s" <$> getCurrentTime
  toLFMStatus <$> Track.scrobble
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


toLFMStatus ∷ Either α β → LFMOperationStatus
toLFMStatus =
  either (const OperationFailed) (const OperationDone)

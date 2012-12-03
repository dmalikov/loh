module Loh.Core.LastFM.Method
  ( nowPlaying, scrobbleTrack
  ) where

import           Control.Applicative       ((<$>))
import           Data.Time                 (formatTime, getCurrentTime)
import           Network.Lastfm            (Lastfm, Response)
import           System.Locale             (defaultTimeLocale)

import qualified Network.Lastfm            as LFM
import qualified Network.Lastfm.JSON.Track as Track

import           Loh.Core.LastFM.Auth
import           Loh.Core.Task


nowPlaying ∷ LFMConfig → Track → Lastfm Response
nowPlaying (ak, sk, s) (Track al ar l t) =
  Track.updateNowPlaying
    (LFM.Artist ar)
    (LFM.Track t)
    (LFM.Album <$> al)
    Nothing
    Nothing
    Nothing
    Nothing
    (LFM.Duration <$> l)
    ak sk s

scrobbleTrack ∷ LFMConfig → Track → Lastfm Response
scrobbleTrack (ak, sk, s) (Track al ar l t) = do
  nts ← read . formatTime defaultTimeLocale "%s" <$> getCurrentTime
  Track.scrobble
    ( LFM.Timestamp nts
    , LFM.Album <$> al
    , LFM.Artist ar
    , LFM.Track t
    , Nothing
    , LFM.Duration <$> l
    , Nothing
    , Nothing
    , Nothing
    , Nothing
    , Nothing
    ) ak sk s

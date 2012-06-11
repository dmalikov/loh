{-# LANGUAGE UnicodeSyntax #-}
module Loh.Scrobbler
  ( nowPlaying, scrobbleDB, scrobbleTrack
  ) where

import Control.Applicative ((<$>))
import Control.Arrow (left)
import Control.Monad (forM_, void)
import Data.Time (formatTime, getCurrentTime)
import System.Locale (defaultTimeLocale)

import qualified Network.Lastfm as LFM
import qualified Network.Lastfm.XML.Track as Track

import Loh.DB (getDB)
import Loh.Types

nowPlaying ∷ LFMConfig → TrackInfo → IO ()
nowPlaying (ak, sk, s) ti =
  void $ left (error . show) <$>
    Track.updateNowPlaying (LFM.Artist $ artist ti)
                           (LFM.Track $ track ti)
                           (Just $ LFM.Album $ album ti)
                           Nothing
                           Nothing
                           Nothing
                           Nothing
                           Nothing
                           ak sk s

scrobbleTrack ∷ LFMConfig → TrackInfo → IO ()
scrobbleTrack (ak, sk, s) ti = do
    nts ← read . formatTime defaultTimeLocale "%s" <$> getCurrentTime
    void $ left (error . show) <$>
        Track.scrobble ( LFM.Timestamp nts
                       , Just $ LFM.Album $ album ti
                       , LFM.Artist $ artist ti
                       , LFM.Track $ track ti
                       , Nothing
                       , Nothing
                       , Nothing
                       , Nothing
                       , Nothing
                       , Nothing
                       , Nothing)
                       ak sk s

scrobbleDB ∷ LFMConfig → IO ()
scrobbleDB (ak, sk, s) = do
  db ← getDB
  forM_ db $ \(DBRecord (Timestamp ts) ti) →
    void $ left (error . show) <$>
        Track.scrobble ( LFM.Timestamp ts
                       , Just $ LFM.Album $ album ti
                       , LFM.Artist $ artist ti
                       , LFM.Track $ track ti
                       , Nothing
                       , Nothing
                       , Nothing
                       , Nothing
                       , Nothing
                       , Nothing
                       , Nothing)
                       ak sk s

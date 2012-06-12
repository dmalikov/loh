module Loh.LoveTrack
  ( loveTrack
  ) where

import Control.Applicative ((<$>))
import Control.Arrow (left)
import Control.Monad (void)

import qualified Network.Lastfm as LFM
import qualified Network.Lastfm.XML.Track as Track

import Loh.Types

loveTrack ∷ LFMConfig → TrackInfo → IO ()
loveTrack (ak, sk, s) ti =
  void $ left (error . show) <$>
    Track.love (LFM.Artist $ artist ti)
               (LFM.Track $ track ti)
               ak sk s

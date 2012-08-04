module Loh.Core.Players.Mocp (getMocpInfo) where

import Control.Applicative ((<$>))
import Control.Monad (mfilter)

import Loh.Core.Players.Kludges
import Loh.Core.Types

import qualified Mocp as MOC


getMocpInfo ∷ IO (Maybe TrackInfo)
getMocpInfo = do
  info ← eitherToMaybe <$> MOC.getMocpInfo
  return $ return . formatMOCTrackInfo =<< MOC.song =<< mfilter isPlaying info
    where isPlaying = (== MOC.Playing) . MOC.state
 

formatMOCTrackInfo ∷ MOC.Song → TrackInfo
formatMOCTrackInfo s = TrackInfo
  { artist = MOC.artist $ MOC.metadata s
  , album = MOC.album $ MOC.metadata s
  , currentSec = MOC.currentSec s
  , totalSec = MOC.totalSec s
  , track = MOC.track $ MOC.metadata s
  }


module Loh.Players.Mpd (getMpdInfo) where

import Control.Arrow (second)
import Control.Applicative ((<$>), (<*>))
import Control.Monad (mfilter)
import Data.Maybe (fromMaybe)

import Loh.Players.Kludges
import Loh.Types

import qualified Data.Map as M
import qualified Network.MPD as MPD

formatMPDTrackInfo ∷ (MPD.Song, (Double, MPD.Seconds)) → TrackInfo
formatMPDTrackInfo (song, (curTime, totalTime)) = TrackInfo
  { artist = fromMaybe "No Artist" $ getTag MPD.Artist
  , album = fromMaybe "No Album" $ getTag MPD.Album
  , currentSec = round curTime
  , totalSec = fromIntegral totalTime
  , track = fromMaybe "No Track" $ getTag MPD.Title
  } where getTag τ = MPD.toString . head <$> M.lookup τ (MPD.sgTags song)

getMpdInfo ∷ IO (Maybe TrackInfo)
getMpdInfo = do
  info ← (liftFstMaybe . eitherToMaybe) <$> getInfo'
  return $ formatMPDTrackInfo . second MPD.stTime <$> mfilter isPlaying info
    where getInfo' = MPD.withMPD $ (,) <$> MPD.currentSong <*> MPD.status
          isPlaying = (== MPD.Playing) . MPD.stState . snd

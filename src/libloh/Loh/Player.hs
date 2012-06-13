module Loh.Player where

import Control.Arrow (second)
import Control.Applicative ((<$>), (<*>))
import Control.Monad (mfilter)
import Data.Function (on)
import Data.Maybe (fromJust, fromMaybe, isJust)

import Loh.Types

import qualified Data.Map as M
import qualified Mocp as MOC
import qualified Network.MPD as MPD

eitherToMaybe ∷ Either α β → Maybe β
eitherToMaybe = either (const Nothing) Just

formatMOCTrackInfo ∷ MOC.Song → TrackInfo
formatMOCTrackInfo s = TrackInfo
  { artist = MOC.artist $ MOC.metadata s
  , album = MOC.album $ MOC.metadata s
  , currentSec = MOC.currentSec s
  , duration = ((/) `on` fromIntegral) (MOC.currentSec s) (MOC.totalSec s)
  , track = MOC.track $ MOC.metadata s
  }

getMocpInfo ∷ IO (Maybe TrackInfo)
getMocpInfo = do
  info ← eitherToMaybe <$> MOC.getMocpInfo
  return $ return . formatMOCTrackInfo =<< MOC.song =<< mfilter isPlaying info
    where
      isPlaying = (== MOC.Playing) . MOC.state

formatMPDTrackInfo ∷ (MPD.Song, (Double, MPD.Seconds)) → TrackInfo
formatMPDTrackInfo (song, (curTime, totalTime)) = TrackInfo
  { artist = fromMaybe "No Artist" $ getTag MPD.Artist
  , album = fromMaybe "No Album" $ getTag MPD.Album
  , currentSec = round curTime
  , duration = curTime / fromIntegral totalTime
  , track = fromMaybe "No Track" $ getTag MPD.Title
  } where
      getTag τ = MPD.toString . head <$> M.lookup τ (MPD.sgTags song)

getMpdInfo ∷ IO (Maybe TrackInfo)
getMpdInfo = do
  info ← (liftFstMaybe . eitherToMaybe) <$> getInfo
  return $ formatMPDTrackInfo . getTime <$> mfilter isPlaying info
    where
      getInfo = MPD.withMPD $ (,) <$> MPD.currentSong <*> MPD.status
      liftFstMaybe m = do
        (ma, b) ← m
        a ← ma
        return (a, b)
      isPlaying = (== MPD.Playing) . MPD.stState . snd
      getTime (song, status) = (song, MPD.stTime status)

getPlayersInfo ∷ IO PlayersInfo
getPlayersInfo = do
  maybeMpdInfo ← getMpdInfo
  maybeMocpInfo ← getMocpInfo
  return . M.fromList . catMaybeSnd $
    [ (Mpd, maybeMpdInfo)
    , (Mocp, maybeMocpInfo)
    ]
    where catMaybeSnd = map (second fromJust) . filter (isJust . snd)

getCurrentTracks ∷ IO [TrackInfo]
getCurrentTracks = M.elems <$> getPlayersInfo

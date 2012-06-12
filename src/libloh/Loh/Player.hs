module Loh.Player where

import Control.Arrow (second)
import Control.Applicative ((<$>))
import Control.Monad (mfilter)
import Data.Function (on)
import Data.Maybe (fromJust, fromMaybe, isJust)

import Loh.Types

import qualified Data.Map as M
import qualified Mocp as MOC
import qualified Network.MPD as MPD

getMocpInfo ∷ IO (Maybe TrackInfo)
getMocpInfo = do
  mocpResponse ← MOC.mocp
  return $ case mocpResponse of
    Right (MOC.MocpInfo MOC.Playing (Just s)) → Just TrackInfo
      { artist = MOC.artist $ MOC.metadata s
      , album = MOC.album $ MOC.metadata s
      , currentSec = MOC.currentSec s
      , duration = ((/) `on` fromIntegral) (MOC.currentSec s) (MOC.totalSec s)
      , track = MOC.track $ MOC.metadata s
      }
    _ → Nothing

getMpdInfo ∷ IO (Maybe TrackInfo)
getMpdInfo = do
  info ← (liftFstMaybe . toMaybe) <$> getInfo
  return $ mfilter isPlaying info >>= getTime >>= formatTrackInfo
    where getInfo = MPD.withMPD $ do
            maybeSong ← MPD.currentSong
            status ← MPD.status
            return (maybeSong, status)
          toMaybe = either (const Nothing) Just
          liftFstMaybe m = do { (ma, b) <- m ; a <- ma ; return (a, b) }
          isPlaying (_, status) = (MPD.stState $ status) == MPD.Playing
          getTime (song',status) = Just (song', MPD.stTime $ status)
          formatTrackInfo (song, (curTime, totalTime)) = Just $ TrackInfo
            { artist = fromMaybe "No Artist" $ MPD.toString . head <$> M.lookup MPD.Artist tag
            , album = fromMaybe "No Album" $ MPD.toString . head <$> M.lookup MPD.Album tag
            , currentSec = round curTime
            , duration = curTime / fromIntegral totalTime
            , track = fromMaybe "No Track" $ MPD.toString . head <$> M.lookup MPD.Title tag
            } where tag = MPD.sgTags song

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

{-# LANGUAGE UnicodeSyntax #-}
module Loh.Player where

import Control.Arrow (second)
import Control.Applicative ((<$>))
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
      , duration = fromIntegral (MOC.currentSec s) / fromIntegral (MOC.totalSec s)
      , track = MOC.track $ MOC.metadata s
      }
    _ → Nothing

getMpdInfo ∷ IO (Maybe TrackInfo)
getMpdInfo = do
  maybeSong ← MPD.withMPD MPD.currentSong
  status ← MPD.withMPD MPD.status
  return $ case status of
    Right _ → case MPD.stState <$> status of
      Right MPD.Playing → case maybeSong of
        Right (Just song) → case MPD.stTime <$> status of
          Right (curTime, totalTime) → Just TrackInfo
            { artist = fromMaybe "No Artist" $ head <$> M.lookup MPD.Artist tag
            , album = fromMaybe "No Album" $ head <$> M.lookup MPD.Album tag
            , currentSec = round curTime
            , duration = curTime / fromIntegral totalTime
            , track = fromMaybe "No Track" $ head <$> M.lookup MPD.Title tag
            } where tag = MPD.sgTags song
          _ → Nothing
        _ → Nothing
      _ → Nothing
    _ → Nothing

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

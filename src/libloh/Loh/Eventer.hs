module Loh.Eventer (eventer) where

import Control.Concurrent (threadDelay)
import Data.Function (on)

import Loh.DB
import Loh.Log
import Loh.InfoMocp
import Loh.InfoMpd
import Loh.Scrobbler
import Loh.Types

toSecs ∷ Int → Int
toSecs = (* 1000000)

threadDelayS ∷ Int → IO ()
threadDelayS = threadDelay . toSecs

fetchDelay ∷ Int
fetchDelay = 20 {- delay between players info fetching, secs -}

fetchAccur ∷ Int
fetchAccur = 3

servePlayer ∷ LFMConfig → IO (Maybe TrackInfo) → Maybe TrackInfo → IO α
servePlayer c getPlayerInfo maybeOldTrack = do
  logMessage $ show maybeOldTrack
  threadDelayS fetchDelay
  logMessage "delay ended"
  maybeNewTrack ← getPlayerInfo
  case maybeNewTrack of
    Just new | maybeNewTrack == maybeOldTrack → do
      stnp ← nowPlaying c new
      case stnp of
        ScrobbleDone → logNowPlaying new
        ScrobbleFailed → logNowPlayingFailed new
      -- if there is a point of waiting to scrobble
      if 2 * currentSec new >= totalSec new
        then servePlayer c getPlayerInfo maybeNewTrack
        else do
          let delayToScrobble = round . (* 0.51) . toRational $ totalSec new
          logMessage $ "waiting " ++ show delayToScrobble ++ " toScrobble"
          threadDelayS delayToScrobble
          trackInfo ← getPlayerInfo
          case trackInfo of
            Just freshNew | new == freshNew && (subtract `on` currentSec) freshNew new < delayToScrobble + fetchAccur → do
              st ← scrobbleTrack c freshNew
              case st of
                ScrobbleDone → logScrobble freshNew
                ScrobbleFailed → do
                  logScrobbleFailed freshNew
                  logDBStore freshNew
                  store freshNew
              servePlayer c getPlayerInfo Nothing
            _ → servePlayer c getPlayerInfo trackInfo
    _ → servePlayer c getPlayerInfo maybeNewTrack

eventer ∷ LFMConfig → IO ()
eventer c = servePlayer c getMpdInfo Nothing

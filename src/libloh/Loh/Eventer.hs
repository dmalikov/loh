module Loh.Eventer (eventer) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever)
import Data.Function (on)

import Loh.DB
import Loh.Log
import Loh.InfoMocp
import Loh.InfoMpd
import Loh.Scrobbler
import Loh.Types

threadDelayS ∷ Int → IO ()
threadDelayS = threadDelay . toSecs
  where toSecs = (* 1000000)

fetchDelay ∷ Int
fetchDelay = 10 {- delay between players info fetching, secs -}

fetchAccur ∷ Int
fetchAccur = 3

-- | Fetch player for specific trackInfo until it will be able to scrobble
followUntilReadyToScrobble ∷ IO (Maybe TrackInfo) → TrackInfo → Int → IO Bool
followUntilReadyToScrobble getPlayerInfo ti timeToFollow
  | timeToFollow < 0 = return True
  | otherwise = do
    logMessage $ "following... " ++ show timeToFollow ++ " to complete"
    threadDelayS fetchDelay
    trackInfo' ← getPlayerInfo
    case trackInfo' of
      Nothing → return False
      Just newti → do
        let diff = (subtract `on` currentSec) ti newti ∷ Int
        if newti == ti && abs (diff - fetchDelay) < fetchAccur
          then followUntilReadyToScrobble getPlayerInfo newti $ timeToFollow - fetchDelay
          else return False

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
          isSameTrack ← followUntilReadyToScrobble getPlayerInfo new delayToScrobble
          if isSameTrack
            then do
              st ← scrobbleTrack c new
              case st of
                ScrobbleDone → logScrobble new
                ScrobbleFailed → do
                  logScrobbleFailed new
                  logDBStore new
                  store new
              servePlayer c getPlayerInfo Nothing
            else
              servePlayer c getPlayerInfo Nothing
    _ → servePlayer c getPlayerInfo maybeNewTrack

eventer ∷ LFMConfig → IO ()
eventer c = do
  mapM_ (forkIO . (\ρ → servePlayer c ρ Nothing)) $
    [ getMocpInfo
    , getMpdInfo
    ]
  forever $ threadDelayS 1

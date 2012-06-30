module Loh.Eventer (eventer) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever, void)
import Data.Function (on)

import Loh.Config
import Loh.DB
import Loh.Log
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
followUntilReadyToScrobble ∷ Player → TrackInfo → Int → IO Bool
followUntilReadyToScrobble ρ ti timeToFollow
  | timeToFollow < 0 = return True
  | otherwise = do
    logMessage ρ $ "following... " ++ show timeToFollow ++ " to complete"
    threadDelayS fetchDelay
    trackInfo' ← getInfo ρ
    case trackInfo' of
      Nothing → return False
      Just newti → do
        let diff = (subtract `on` currentSec) ti newti ∷ Int
        if newti == ti && abs (diff - fetchDelay) < fetchAccur
          then followUntilReadyToScrobble ρ newti $ timeToFollow - fetchDelay
          else return False

servePlayer ∷ LFMConfig → Player → Maybe TrackInfo → IO ()
servePlayer c ρ maybeOldTrack = do
  logMessage ρ $ "currect track is " ++ show maybeOldTrack
  threadDelayS fetchDelay
  maybeNewTrack ← getInfo ρ
  case maybeNewTrack of
    Just new | maybeNewTrack == maybeOldTrack → do
      stnp ← nowPlaying c new
      case stnp of
        ScrobbleDone → logNowPlaying new
        ScrobbleFailed → logNowPlayingFailed new
      -- if there is a point of waiting to scrobble
      if 2 * currentSec new >= totalSec new
        then servePlayer c ρ maybeNewTrack
        else do
          let delayToScrobble = round . (* 0.51) . toRational $ totalSec new
          logMessage ρ $ "waiting " ++ show delayToScrobble ++ " toScrobble"
          isSameTrack ← followUntilReadyToScrobble ρ new delayToScrobble
          if isSameTrack
            then do
              st ← scrobbleTrack c new
              case st of
                ScrobbleDone → logScrobble new
                ScrobbleFailed → do
                  logScrobbleFailed new
                  logDBStore new
                  store new
              servePlayer c ρ Nothing
            else
              servePlayer c ρ Nothing
    _ → servePlayer c ρ maybeNewTrack

eventer ∷ LFMConfig → IO ()
eventer c = do
  mapM_ (void . forkIO . (\ρ → servePlayer c ρ Nothing)) =<< getPlayers
  forever $ threadDelayS 1

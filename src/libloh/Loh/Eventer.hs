module Loh.Eventer (eventer) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever, void)
import Data.Function (on)

import Loh.Config (LConfig(..), readConfig, writeConfig)
import Loh.DB
import Loh.LastFM.Method
import Loh.Log
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
    -- logMessageP ρ $ "following... " ++ show timeToFollow ++ " to complete"
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
  -- logMessageP ρ $ "currect track is " ++ show maybeOldTrack
  threadDelayS fetchDelay
  maybeNewTrack ← getInfo ρ
  case maybeNewTrack of
    Just new | maybeNewTrack == maybeOldTrack → do
      -- if there is a point of waiting to scrobble
      if 2 * currentSec new >= totalSec new
        then servePlayer c ρ maybeNewTrack
        else do
          stnp ← nowPlaying c new
          case stnp of
            OperationDone → logNowPlaying ρ new
            OperationFailed → logNowPlayingFailed ρ new
          let delayToScrobble = round . (* 0.51) . toRational $ totalSec new
          -- logMessageP ρ $ "waiting " ++ show delayToScrobble ++ " toScrobble"
          isSameTrack ← followUntilReadyToScrobble ρ new delayToScrobble
          if isSameTrack
            then do
              st ← scrobbleTrack c new
              case st of
                OperationDone → logScrobble ρ new
                OperationFailed → do
                  logScrobbleFailed ρ new
                  logDBStore ρ new
                  store new
              servePlayer c ρ Nothing
            else
              servePlayer c ρ Nothing
    _ → servePlayer c ρ maybeNewTrack

eventer ∷ IO ()
eventer = do
  config ← readConfig
  let c = lfmConfig config
      ps = players config
  writeConfig config
  putStrLn $ "Start scrobbling " ++ (show $ map name ps)
  mapM_ (void . forkIO . (\ρ → servePlayer c ρ Nothing)) ps
  forever $ threadDelayS 1

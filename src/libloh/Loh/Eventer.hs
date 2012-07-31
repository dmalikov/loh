module Loh.Eventer (eventer) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever, forM_, void)
import Data.Aeson (encode)
import Data.Function (on)
import Network
import System.IO
import System.Log.Logger (debugM, infoM, warningM)
import Text.Printf (printf)

import qualified Data.ByteString.Lazy.Char8 as BS

import Loh.Config (LConfig(..))
import Loh.LastFM.Method
import Loh.Scrobbler (scrobbler)
import Loh.Types


eventer ∷ LConfig → IO ()
eventer config =
  if null players'
    then error $ unlines
           [ "There is nothing to scrobble!"
           , "Add some players to your config file."
           ]
    else do
      void . forkIO . scrobbler . lfmConfig $ config
      infoM "Loh.Eventer" $ "Start scrobbling " ++ show (map name players')
      forM_ players' $ \ρ → withSocketsDo $ do
        h <- connectTo "127.0.0.1" (PortNumber lohPort)
        hSetBuffering h LineBuffering
        void . forkIO $ servePlayer c h ρ Nothing
      forever $ threadDelayS 1
  where
    c = lfmConfig config
    players' = players config


threadDelayS ∷ Int → IO ()
threadDelayS = threadDelay . (* 1000000)

fetchDelay ∷ Int
fetchDelay = 10 {- delay between players info fetching, secs -}

fetchAccur ∷ Int
fetchAccur = 3

-- | Fetch player for specific trackInfo until it will be able to scrobble
followUntilReadyToScrobble ∷ Player → TrackInfo → Int → IO Bool
followUntilReadyToScrobble ρ ti timeToFollow
  | timeToFollow < 0 = return True
  | otherwise = do
    debugM "Loh.Eventer" $ logMessageP ρ ("following... " ++ show timeToFollow ++ " to complete")
    threadDelayS fetchDelay
    trackInfo' ← getInfo ρ
    case trackInfo' of
      Nothing → return False
      Just newti → do
        let diff = (subtract `on` currentSec) ti newti ∷ Int
        if newti == ti && abs (diff - fetchDelay) < fetchAccur
          then followUntilReadyToScrobble ρ newti $ timeToFollow - fetchDelay
          else return False

servePlayer ∷ LFMConfig → Handle → Player → Maybe TrackInfo → IO ()
servePlayer c h ρ maybeOldTrack = do
  threadDelayS fetchDelay
  maybeNewTrack ← getInfo ρ
  case maybeNewTrack of
    Just new | maybeNewTrack == maybeOldTrack →
      -- if there is a point of waiting to scrobble
      if 2 * currentSec new >= totalSec new
        then servePlayer c h ρ maybeNewTrack
        else do
          stnp ← nowPlaying c new
          case stnp of
            Right _ → debugM "Loh.Eventer" $ logNowPlaying new
            Left _ → warningM "Loh.Eventer" $ logNowPlayingFailed new
          let delayToScrobble = round . (* 0.51) . toRational $ totalSec new
          debugM "Loh.Eventer" $ logMessageP ρ ("waiting " ++ show delayToScrobble ++ " toScrobble")
          isSameTrack ← followUntilReadyToScrobble ρ new delayToScrobble
          if isSameTrack
            then do
              BS.hPut h $ encode new
              servePlayer c h ρ Nothing
            else
              servePlayer c h ρ Nothing
    _ → servePlayer c h ρ maybeNewTrack
 where
  logNowPlaying = log_ "[%s] now playing \"%s - %s\""

  logNowPlayingFailed = log_ "[%s] now playing \"%s - %s\" failed"

  log_ format τ = printf format (show $ name ρ) (artist τ) (track τ)

logMessageP ∷ Player → String → String
logMessageP ρ s = printf "[%s] %s" (show $ name ρ) s

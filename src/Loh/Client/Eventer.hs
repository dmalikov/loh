module Loh.Client.Eventer (eventer) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever, forM_, void, when)
import Data.Aeson (encode)
import Data.Function (on)
import Network
import System.IO
import System.Log.Logger (debugM, infoM, warningM)
import Text.Printf (printf)

import qualified Data.ByteString.Lazy.Char8 as BS

import Loh.Client.Config (LConfig(..))
import Loh.Core.LastFM.Method
import Loh.Core.Types


eventer ∷ LConfig → IO ()
eventer config =
  if null players'
    then error $ unlines
           [ "There is nothing to scrobble!"
           , "Add some players to your config file."
           ]
    else do
      infoM "Eventer" $ "Start watching " ++ show (map name players')
      forM_ players' $ \ρ → withSocketsDo $ do
        h <- connectTo (serverHost config) (PortNumber lohPort)
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
    debugM "Eventer" $ logMessageP ρ ("following... " ++ show timeToFollow ++ " secs to scrobble")
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
          BS.hPut h $ encode $ Task UpdateNowPlaying c new
          let delayToScrobble = round . (* 0.51) . toRational $ totalSec new
          debugM "Eventer" $ logMessageP ρ ("start waiting " ++ show delayToScrobble ++ " secs to scrobble")
          isSameTrack ← followUntilReadyToScrobble ρ new delayToScrobble
          when isSameTrack $ BS.hPut h $ encode $ Task Scrobble c new
          servePlayer c h ρ Nothing
    _ → servePlayer c h ρ maybeNewTrack

logMessageP ∷ Player → String → String
logMessageP ρ = printf "[%s] %s" (show $ name ρ)

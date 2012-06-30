module Loh.Log
  ( logNowPlaying
  , logNowPlayingFailed
  , logScrobble
  , logScrobbleFailed
  , logDBStore
  , logMessage
  ) where

import Control.Applicative ((<$>))
import Data.Time (formatTime, getCurrentTime)
import System.Locale (defaultTimeLocale)
import Text.Printf

import Loh.Format
import Loh.Types

getTime ∷ IO String
getTime = formatTime defaultTimeLocale timeFormat <$> getCurrentTime

logMessage ∷ Player → String → IO ()
logMessage ρ s = do
  time ← getTime
  printf logMessageFormat time (show $ name ρ) s

log_ ∷ String → TrackInfo → IO ()
log_ formatString τ = do
  time ← getTime
  printf formatString time (artist τ) (track τ)

logNowPlaying ∷ TrackInfo → IO ()
logNowPlaying = log_ logNowPlayingFormat

logNowPlayingFailed ∷ TrackInfo → IO ()
logNowPlayingFailed = log_ logNowPlayingFailedFormat

logScrobble ∷ TrackInfo → IO ()
logScrobble = log_ logScrobbleFormat

logScrobbleFailed ∷ TrackInfo → IO ()
logScrobbleFailed = log_ logScrobbleFailedFormat

logDBStore ∷ TrackInfo → IO ()
logDBStore = log_ logDBStoreFormat

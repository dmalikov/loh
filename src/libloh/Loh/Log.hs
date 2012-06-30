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

log_ ∷ String → Player → TrackInfo → IO ()
log_ formatString ρ τ = do
  time ← getTime
  printf formatString time (show $ name ρ) (artist τ) (track τ)

logNowPlaying ∷ Player → TrackInfo → IO ()
logNowPlaying = log_ logNowPlayingFormat

logNowPlayingFailed ∷ Player → TrackInfo → IO ()
logNowPlayingFailed = log_ logNowPlayingFailedFormat

logScrobble ∷ Player → TrackInfo → IO ()
logScrobble = log_ logScrobbleFormat

logScrobbleFailed ∷ Player → TrackInfo → IO ()
logScrobbleFailed = log_ logScrobbleFailedFormat

logDBStore ∷ Player → TrackInfo → IO ()
logDBStore = log_ logDBStoreFormat

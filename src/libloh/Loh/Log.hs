{-# LANGUAGE UnicodeSyntax #-}
module Loh.Log
  ( logNowPlaying
  , logScrobble
  ) where

import Control.Applicative ((<$>))
import Data.Time (formatTime, getCurrentTime)
import System.Locale (defaultTimeLocale)
import Text.Printf

import Loh.Types

getTime ∷ IO String
getTime = formatTime defaultTimeLocale "%T" <$> getCurrentTime

logNowPlaying ∷ TrackInfo → IO ()
logNowPlaying ti = do
  time ← getTime
  printf "%s: Now Playing: %s - %s\n" time (artist ti) (track ti)

logScrobble ∷ TrackInfo → IO ()
logScrobble ti = do
  time ← getTime
  printf "%s: Scrobble: %s - %s\n" time (artist ti) (track ti)

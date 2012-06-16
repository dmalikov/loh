module Loh.Format where

logNowPlayingFormat ∷ String
logNowPlayingFormat = "%s: now playing \"%s - %s\"\n"

logNowPlayingFailedFormat ∷ String
logNowPlayingFailedFormat = "%s: now playing \"%s - %s\" failed\n"

logScrobbleFormat ∷ String
logScrobbleFormat = "%s: scrobbling \"%s - %s\"\n"

logScrobbleFailedFormat ∷ String
logScrobbleFailedFormat = "%s: scrobbling \"%s - %s\" failed \n"

logDBStoreFormat ∷ String
logDBStoreFormat = "%s: stored in DB \"%s - %s\"\n"

logMessageFormat ∷ String
logMessageFormat = "%s: %s\n"

timeFormat ∷ String
timeFormat = "%F [%T]"

module Loh.Format where

logNowPlayingFormat ∷ String
logNowPlayingFormat = "%s: [%s] now playing \"%s - %s\"\n"

logNowPlayingFailedFormat ∷ String
logNowPlayingFailedFormat = "%s: [%s] now playing \"%s - %s\" failed\n"

logScrobbleFormat ∷ String
logScrobbleFormat = "%s: [%s] scrobbling \"%s - %s\"\n"

logScrobbleFailedFormat ∷ String
logScrobbleFailedFormat = "%s: [%s] scrobbling \"%s - %s\" failed \n"

logDBStoreFormat ∷ String
logDBStoreFormat = "%s: [%s] stored in DB \"%s - %s\"\n"

logMessageFormat ∷ String
logMessageFormat = "%s: [%s] %s\n"

timeFormat ∷ String
timeFormat = "%F [%T]"

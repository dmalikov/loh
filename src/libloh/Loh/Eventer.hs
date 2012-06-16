module Loh.Eventer (eventer) where

import Control.Concurrent (threadDelay)
import Data.Function (on)
import Data.Traversable (sequenceA)

import qualified Data.Map as M

import Loh.DB
import Loh.Log
import Loh.Player
import Loh.Scrobbler
import Loh.Types

fetchDelay ∷ Int
fetchDelay = 20 {- delay between players info fetching, secs -}

fetchAccur ∷ Int
fetchAccur = 3

scrobbleDuration ∷ Duration
scrobbleDuration = 0.51

isConsistent ∷ TrackInfo → TrackInfo → Bool
isConsistent α β =
  α == β && abs (((-) `on` currentSec) α β) < fetchDelay + fetchAccur

isReadyToScrobble ∷ Maybe Duration → TrackInfo → Bool
isReadyToScrobble Nothing _ = False
isReadyToScrobble (Just startDuration) ti =
  duration ti - startDuration > scrobbleDuration

manageTrackInfo ∷ LFMConfig
                → TrackInfo
                → Maybe (TrackInfo, Maybe Duration)
                → IO (TrackInfo, Maybe Duration)
manageTrackInfo config new Nothing = do
    _ ← nowPlaying config new
    return (new, Just $ duration new)
manageTrackInfo config new (Just (old, d)) =
  if isConsistent old new
    then do
      -- logMessage "Trying update now playing"
      stnp ← nowPlaying config new
      -- logMessage "Finished updating now playing"
      case stnp of
        ScrobbleDone → logNowPlaying new
        ScrobbleFailed → logNowPlayingFailed new
      if isReadyToScrobble d new
        then do
          -- logMessage "Trying scroblle"
          st ← scrobbleTrack config new
          -- logMessage "Finished scrobble"
          case st of
            ScrobbleDone → logScrobble new
            ScrobbleFailed → do
              logScrobbleFailed new
              logDBStore new
              store new
          return (new, Nothing)
        else
          return (new, d)
    else
      return (new, Just $ duration new)

intersect ∷ Ord k ⇒ (α → Maybe β → γ) → M.Map k α → M.Map k β → M.Map k γ
intersect f ma mb = M.mapWithKey (\k a → f a (M.lookup k mb)) ma

updateCurrentTracks ∷ LFMConfig
                    → PlayersInfo
                    → PlayersInfoToScrobble
                    → IO PlayersInfoToScrobble
updateCurrentTracks c newP oldP =
  sequenceA $ intersect (manageTrackInfo c) newP oldP

eventer ∷ LFMConfig → IO α
eventer c = eventer' c M.empty
  where
    eventer' config currentTracks = do
      threadDelay $ fetchDelay*1000000
      players ← getPlayersInfo
      updatedTracks ← updateCurrentTracks config players currentTracks
      eventer' config updatedTracks

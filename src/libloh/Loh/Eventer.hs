{-# LANGUAGE UnicodeSyntax #-}
module Loh.Eventer (eventer) where

import Control.Concurrent (threadDelay)
import Data.Function (on)
import System.IO.Unsafe

import qualified Data.Map as M

import Loh.Player
import Loh.Scrobbler
import Loh.Types

fetchDelay ∷ Int
fetchDelay = 20 {- delay between players info fetching, sec -}

fetchAccur ∷ Int
fetchAccur = 3

scrobbleDuration ∷ Duration
scrobbleDuration = 0.51

isConsistent ∷ TrackInfo → TrackInfo → Bool
isConsistent α β = α == β && abs (((-) `on` currentSec) α β) < fetchDelay + fetchAccur

isReadyToScrobble ∷ Maybe Duration → TrackInfo → Bool
isReadyToScrobble Nothing _ = False
isReadyToScrobble (Just startDuration) ti =
  duration ti - startDuration > scrobbleDuration

manageTrackInfo ∷ LFMConfig → TrackInfo → Maybe (TrackInfo, Maybe Duration) → (TrackInfo, Maybe Duration)
manageTrackInfo config new Nothing = unsafePerformIO $ do
    nowPlaying config new
    return (new, Just $ duration new)
manageTrackInfo config new (Just (old, d)) = unsafePerformIO $ do
  if isConsistent old new
    then do
      nowPlaying config new
      if isReadyToScrobble d new
        then do
          scrobbleTrack config new
          return (new, Nothing)
        else do
          return (new, d)
    else do
      return (new, Just $ duration new)

intersect ∷ Ord k ⇒ (α → Maybe β → γ) → M.Map k α → M.Map k β → M.Map k γ
intersect f ma mb = M.mapWithKey (\k a → f a (M.lookup k mb)) ma

updateCurrentTracks ∷ LFMConfig → PlayersInfo → PlayersInfoToScrobble → PlayersInfoToScrobble
updateCurrentTracks c = intersect (manageTrackInfo c)

eventer ∷ LFMConfig → IO α
eventer c = eventer' c M.empty
  where
    eventer' config currentTracks = do
      threadDelay $ fetchDelay*1000000
      players ← getPlayersInfo
      putStrLn $ "currentData: " ++ show currentTracks
      putStrLn $ "get some info:  " ++ show players ++ "\n"
      eventer' config $ updateCurrentTracks config players currentTracks

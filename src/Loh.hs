{-# LANGUAGE UnicodeSyntax #-}
module Loh where

import Control.Applicative ((<$>), (<*>))
import Control.Concurrent (threadDelay)
import Control.Monad (forever, liftM2)
import Data.Char (isSpace)
import Data.Function (on)
import Data.List.Split (splitOn)
import Data.Maybe
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))
import System.IO.Unsafe

import qualified Data.Map as M
import qualified Network.Lastfm as LFM

import Loh.Player
import Loh.Scrobbler
import Loh.Types

configFilePath ∷ FilePath
configFilePath = ".lastfm.conf"

fetchDelay ∷ Int
fetchDelay = 20 {- delay between players info fetching, sec -}

fetchAccur ∷ Int
fetchAccur = 3

scrobbleDuration ∷ Duration
scrobbleDuration = 0.51

getConfig ∷ IO LFMConfig
getConfig = do
  configContent ← readFile <$> (</> configFilePath) <$> getHomeDirectory
  values ← map ((\[x,y] -> (x,y)) . splitOn "=" . filter (not . isSpace)) . lines <$> configContent
  return $ fromMaybe (error "Config at ~/.lastfm.conf should contain APIKey, SessionKey and Secret") (readConfig values)
  where
    readConfig xs = (,,) <$> (LFM.APIKey <$> lookup "APIKey" xs) <*> (LFM.SessionKey <$> lookup "SessionKey" xs) <*> (LFM.Secret <$> lookup "Secret" xs)

isConsistent ∷ TrackInfo → TrackInfo → Bool
isConsistent α β = and [ α == β
                       , abs (((-) `on` currentSec) α β) < fetchDelay + fetchAccur
                       ]

isReadyToScrobble ∷ Maybe Duration → TrackInfo → Bool
isReadyToScrobble Nothing _ = False
isReadyToScrobble (Just startDuration) ti =
  duration ti - startDuration > scrobbleDuration

manageTrackInfo ∷ LFMConfig → TrackInfo → Maybe (TrackInfo, Maybe Duration) → (TrackInfo, Maybe Duration)
manageTrackInfo config new Nothing = unsafePerformIO $ do
    putStrLn "Second is Nothing"
    nowPlaying config new
    putStrLn $ "Now playing" ++ show new
    return (new, Just $ duration new)
manageTrackInfo config new (Just (old, d)) = unsafePerformIO $ do
  if isConsistent old new
    then do
      nowPlaying config new
      putStrLn $ "Now playing" ++ show new
      if isReadyToScrobble d new
        then do
          scrobbleTrack config new
          putStrLn $ "Scrobbling" ++ show new
          return (new, Nothing)
        else do
          print $ liftM2 (-) (Just $ duration new) d
          print $ scrobbleDuration
          return (new, d)
    else do
      putStrLn "track changed"
      return (new, Just $ duration new)

intersect ∷ Ord k ⇒ (α → Maybe β → γ) → M.Map k α → M.Map k β → M.Map k γ
intersect f ma mb = M.mapWithKey (\k a → f a (M.lookup k mb)) ma

updateCurrentTracks ∷ LFMConfig → PlayersInfo → PlayersInfoToScrobble → PlayersInfoToScrobble
updateCurrentTracks c = intersect (manageTrackInfo c)

main ∷ IO ()
main = do
  config ← getConfig
  forever $ go config M.empty
  where
    go config currentTracks = do
      threadDelay $ fetchDelay*1000000
      players ← getPlayersInfo
      putStrLn $ "currentData: " ++ show currentTracks
      putStrLn $ "get some info:  " ++ show players ++ "\n"
      go config $ updateCurrentTracks config players currentTracks
 

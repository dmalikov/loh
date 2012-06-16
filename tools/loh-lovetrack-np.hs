module Main where

import Control.Monad (forM_)
import Text.Printf

import Loh.Config
import Loh.LoveTrack
import Loh.Player
import Loh.Types

main ∷ IO ()
main = do
  config ← getConfig
  currentTracks ← getCurrentTracks
  if null currentTracks
    then putStrLn "Error: Nothing to love"
    else forM_ currentTracks $ \τ → do
      loveTrack config τ
      printf "Loved \"%s - %s\"\n" (artist τ) (track τ)

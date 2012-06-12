{-# LANGUAGE UnicodeSyntax #-}
module Main where

import Control.Monad (forM_)

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
    else forM_ currentTracks $ \t → do
      loveTrack config t
      putStrLn $ "Loved " ++ artist t ++ " - " ++ track t

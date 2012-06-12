#!/usr/bin/env runhaskell

import Loh.Config
import Loh.LoveTrack
import Loh.Player

main :: IO ()
main = do
  config <- getConfig
  mapM_ (loveTrack config) =<< getCurrentTracks


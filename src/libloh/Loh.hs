module Main where

import Loh.Eventer
import Loh.LastFM.Config

main ∷ IO ()
main = eventer =<< getConfig

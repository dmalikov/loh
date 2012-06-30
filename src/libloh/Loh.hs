module Main where

import Loh.LFMConfig
import Loh.Eventer

main ∷ IO ()
main = eventer =<< getConfig

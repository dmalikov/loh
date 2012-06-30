module Main where

import Loh.Config
import Loh.Eventer

main ∷ IO ()
main = eventer =<< getConfig

module Main where

import Loh.Config (readConfig)
import Loh.Eventer

main ∷ IO ()
main = eventer =<< readConfig

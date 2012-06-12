{-# LANGUAGE UnicodeSyntax #-}
module Main where

import Control.Monad (forever)

import Loh.Config
import Loh.Eventer

main ∷ IO ()
main = forever $ eventer =<< getConfig

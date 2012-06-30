module Loh.Config (getPlayers) where

import System.Directory (getHomeDirectory)
import System.FilePath ((</>))

import Loh.Players
import Loh.Types

configFileName ∷ String
configFileName = ".lohrc"

getPlayers ∷ IO [Player]
getPlayers = do
  configContent ← readFile . (</> configFileName) =<< getHomeDirectory
  return . map (fromName . (read ∷ String → PlayerName)) . lines $ configContent

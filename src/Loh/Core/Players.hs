module Loh.Core.Players
  ( fromName
  ) where

import           Loh.Core.Players.Mpd
import           Loh.Core.Types


fromName ∷ PlayerName → Player
fromName Mpd = Player { name = Mpd, getInfo = getMpdInfo }

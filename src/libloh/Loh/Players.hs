module Loh.Players
  ( fromName
  ) where

import Loh.Players.Mocp
import Loh.Players.Mpd
import Loh.Types


fromName ∷ PlayerName → Player
fromName Mocp = Player { name = Mocp, getInfo = getMocpInfo }
fromName Mpd = Player { name = Mpd, getInfo = getMpdInfo }

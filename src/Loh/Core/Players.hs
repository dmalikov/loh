module Loh.Core.Players
  ( fromName
  ) where

import Loh.Core.Players.Mocp
import Loh.Core.Players.Mpd
import Loh.Core.Types


fromName ∷ PlayerName → Player
fromName Mocp = Player { name = Mocp, getInfo = getMocpInfo }
fromName Mpd = Player { name = Mpd, getInfo = getMpdInfo }

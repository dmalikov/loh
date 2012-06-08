{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UnicodeSyntax #-}
module Loh.Types where

import qualified Data.Map as M

newtype Timestamp = Timestamp Integer
  deriving (Read, Show)

newtype DBSize = DBSize Int
  deriving (Read, Show)

data TrackInfo = TrackInfo
  { artist    ∷ String
  , album     ∷ String
  , duration  ∷ Double
  , track     ∷ String
  } deriving (Read, Show)

data DBRecord = DBRecord Timestamp TrackInfo
  deriving (Read, Show)

data PlayerName = Mocp | Mpd
  deriving (Eq, Ord, Read, Show)

type PlayersInfo = M.Map PlayerName TrackInfo

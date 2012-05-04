{-# LANGUAGE TypeSynonymInstances #-}

module Loh.Types
  ( DBRecord(..), DBSize(..), Timestamp(..), TrackInfo(..)
  ) where

newtype Timestamp = Timestamp Integer
  deriving (Read, Show)

newtype DBSize = DBSize Int
  deriving (Read, Show)

data TrackInfo = TrackInfo
  { artist    ∷ String
  , album     ∷ String
  , duration  ∷ Int
  , track     ∷ String
  } deriving (Read, Show)

data DBRecord = DBRecord Timestamp TrackInfo
  deriving (Read, Show)

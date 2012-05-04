module Types
  ( Album, Artist, CurrentSec, File, Title, TotalSec, Track
  , Metadata(..), Song(..), State(..)
  , MocpError, MocpInfo(..), MocpResponse, MocpStatus
  ) where

type MocpResponse = String

data State = Paused | Playing | Stopped
  deriving (Eq, Show)

type Album  = String
type Artist = String
type File   = String
type Title  = String
type Track  = String

data Metadata = Metadata
  { album  ∷ Album
  , artist ∷ Artist
  , file   ∷ File
  , title  ∷ Title
  , track  ∷ Track
  } deriving Show

type CurrentSec = Int
type TotalSec   = Int

data Song = Song
  { metadata   ∷ Metadata
  , currentSec ∷ CurrentSec
  , totalSec   ∷ TotalSec
  } deriving Show

type MocpError = String

data MocpInfo = MocpInfo State (Maybe Song)
  deriving Show

type MocpStatus = Either MocpError MocpInfo

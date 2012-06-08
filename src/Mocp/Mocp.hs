{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE UnicodeSyntax #-}

module Mocp
  ( mocp
  , Metadata(..), MocpInfo(..), MocpState(..), Song(..)
  ) where

import Control.Applicative ((<$>))
import Control.Monad (when)
import Control.Monad.Error (ErrorT(..), runErrorT, throwError)
import Control.Monad.IO.Class (liftIO)
import Data.List (isPrefixOf)
import Data.List.Split (splitOn)
import GHC.IO.Exception (ExitCode(..))
import System.Process (readProcessWithExitCode)

type MocpResponse = Either MocpError

type MocpError = String

data MocpState = Paused | Playing | Stopped
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

data MocpInfo = MocpInfo MocpState (Maybe Song)
  deriving Show

mocp ∷ IO (MocpResponse MocpInfo)
mocp = (>>= getStatus) <$> runErrorT callMocp

callMocp ∷ ErrorT MocpError IO String
callMocp = do
  (exitCode, !response, errorMessage) ← liftIO $ readProcessWithExitCode "mocp" ["-i"] ""
  when (exitCode /= ExitSuccess) $ throwError errorMessage
  return response

getValue ∷ String → String → String
getValue p = (!! 1) . splitOn ": " . head . filter (isPrefixOf p) . lines

getState ∷ String → Maybe MocpState
getState r =
  case getValue "State" r of
    "PAUSE" → Just Paused
    "PLAY"  → Just Playing
    "STOP"  → Just Stopped
    _       → Nothing

getSong ∷ String → Maybe Song
getSong r =
  case getState r of
    Nothing      → Nothing
    Just Stopped → Nothing
    _            → Just Song
      {
       metadata = Metadata
        { album  = getValue "Album" r
        , artist = getValue "Artist" r
        , file   = getValue "File" r
        , title  = getValue "Title" r
        , track  = getValue "SongTitle" r
        }
        , currentSec = read $ getValue "CurrentSec" r
        , totalSec   = read $ getValue "TotalSec" r
      }

getStatus ∷ String → MocpResponse MocpInfo
getStatus r =
  case getState r of
    Nothing      → Left "Mocp server is not running"
    Just Stopped → Right $ MocpInfo Stopped Nothing
    Just s       → Right $ MocpInfo s $ getSong r

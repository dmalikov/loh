{-# LANGUAGE BangPatterns #-}

module Mocp
  ( mocp
  ) where

import Control.Applicative ((<$>))
import Control.Monad (when)
import Control.Monad.Error (ErrorT(..), runErrorT, throwError)
import Control.Monad.IO.Class (liftIO)
import Data.List (isPrefixOf)
import Data.List.Split (splitOn)
import GHC.IO.Exception (ExitCode(..))
import System.Process (readProcessWithExitCode)

import Types

getValue ∷ String → MocpResponse → String
getValue p = (!! 1) . splitOn ": " . head . filter (isPrefixOf p) . lines

getState ∷ MocpResponse → Maybe State
getState r =
  case getValue "State" r of
    "PAUSE" → Just Paused
    "PLAY"  → Just Playing
    "STOP"  → Just Stopped
    _       → Nothing

getSong ∷ MocpResponse → Maybe Song
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

getStatus ∷ MocpResponse → MocpStatus
getStatus r =
  case getState r of
    Nothing      → Left "Mocp server is not running"
    Just Stopped → Right $ MocpInfo Stopped Nothing
    Just s       → Right $ MocpInfo s $ getSong r

mocp ∷ IO MocpStatus
mocp = (>>= getStatus) <$> runErrorT callMocp

callMocp ∷ ErrorT MocpError IO MocpResponse
callMocp = do
  (exitCode, !response, errorMessage) ← liftIO $ readProcessWithExitCode "mocp" ["-i"] ""
  when (exitCode /= ExitSuccess) $ throwError errorMessage
  return response

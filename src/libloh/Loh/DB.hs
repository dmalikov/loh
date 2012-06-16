module Loh.DB
  ( clear
  , getDB
  , store
  ) where

import Control.Applicative ((<$>))
import Data.Time (formatTime, getCurrentTime)
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))
import System.Locale (defaultTimeLocale)

import Loh.Types

dbFilename ∷ FilePath
dbFilename = ".loh.db"

dbFile ∷ IO FilePath
dbFile = (</> dbFilename) <$> getHomeDirectory

store ∷ TrackInfo → IO ()
store ti = do
  ts ← Timestamp <$> read . formatTime defaultTimeLocale "%s" <$> getCurrentTime
  flip appendFile (show (DBRecord ts ti) ++ "\n") =<< dbFile

clear ∷ IO ()
clear = flip writeFile "" =<< dbFile

getDB ∷ IO [DBRecord]
getDB = map read . lines <$> (readFile =<< dbFile)

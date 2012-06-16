module Loh.DB
  ( clear
  , getDB
  , store
  ) where

import Control.Applicative ((<$>))
import Data.Time (getCurrentTime)
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))

import Loh.Types

dbFilename ∷ FilePath
dbFilename = ".loh.db"

dbFile ∷ IO FilePath
dbFile = (</> dbFilename) <$> getHomeDirectory

store ∷ TrackInfo → IO ()
store ti = do
  ts ← getCurrentTime
  flip appendFile (show (DBRecord ts ti) ++ "\n") =<< dbFile

clear ∷ IO ()
clear = flip writeFile "" =<< dbFile

getDB ∷ IO [DBRecord]
getDB = map read . filter (not . null) . lines <$> (readFile =<< dbFile)

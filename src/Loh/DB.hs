{-# LANGUAGE UnicodeSyntax #-}
module Loh.DB
  ( clear
  , getDB
  , getDBSize
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
  dbFile' ← dbFile
  ts ← Timestamp <$> read . formatTime defaultTimeLocale "%s" <$> getCurrentTime
  appendFile dbFile' $ show (DBRecord ts ti) ++ "\n"

clear ∷ IO ()
clear = do
  dbFile' ← dbFile
  writeFile dbFile' ""

getDB ∷ IO [DBRecord]
getDB = do
  records ← lines <$> (readFile =<< dbFile)
  return $ map read records

getDBSize ∷ IO DBSize
getDBSize = (DBSize . length) <$> getDB

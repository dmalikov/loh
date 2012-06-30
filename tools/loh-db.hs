module Main where

import Control.Applicative ((<$>))
import Control.Monad (forM_, forM)
import Data.List (intercalate)
import Data.Time (formatTime)
import System.Environment (getArgs)
import System.Locale (defaultTimeLocale)
import Text.Printf

import Loh.LFMConfig
import Loh.DB
import Loh.Scrobbler
import Loh.Types

dbList ∷ IO ()
dbList = do
  db ← getDB
  printf "%d tracks stored:\n" (length db)
  forM_ db $ \dbRecord → do
    let ti = trackInfo dbRecord
        τ = formatTime defaultTimeLocale "%B %e, %H:%M" $ timestamp dbRecord
    printf "\"%s - %s\" played %s\n" (artist ti) (track ti) τ

dbPush ∷ IO ()
dbPush = do
  config ← getConfig
  db ← getDB
  let recordsNumber = length db
  statuses ← forM (zip db [1..]) $ \((DBRecord ts ti), recordNumber) → do
    status ← scrobbleTrack config ti
    let statusString = case status of
          ScrobbleFailed → "failed"
          ScrobbleDone → "done"
    printf "(%d/%d) scrobbled \"%s - %s\", %s\n"
      (recordNumber∷Int) recordsNumber (artist ti) (track ti) statusString
    return status
  if (not . null . filter (== ScrobbleFailed) $ statuses)
    then
      putStrLn "Total: DB pushing is failed"
    else do
      putStrLn "Total: DB pushing is done"
      clear

dbClear ∷ IO ()
dbClear = do
  clear
  putStrLn "DB is cleared"

dbHelp ∷ IO ()
dbHelp = putStrLn . intercalate "\n" $
  [ "Usage: loh-db [clear | list | push]"
  , ""
  , "\t clear: remove all tracks from DB without scrobbling"
  , ""
  , "\t list: list DB contents"
  , ""
  , "\t push: scrobble all tracks from DB"
  ]

main ∷ IO ()
main = do
  action ← take 1 <$> getArgs
  case action of
    ["clear"] → dbClear
    ["list"] → dbList
    ["push"] → dbPush
    _ → dbHelp

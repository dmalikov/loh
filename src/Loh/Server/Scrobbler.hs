{-# LANGUAGE OverloadedStrings #-}
module Loh.Server.Scrobbler
  ( scrobbler
  ) where

import           Control.Applicative        ((<$>))
import           Control.Concurrent         (forkIO)
import           Control.Monad.Loops        (dropWhileM)
import           Control.Monad.State
import           Data.Aeson                 (decode)
import           Network.Socket
import           System.IO
import           System.Log.Logger          (debugM, infoM, warningM)
import           Text.Printf

import qualified Data.ByteString.Lazy.Char8 as BS

import           Loh.Core.LastFM.Method
import           Loh.Core.Task
import           Loh.Core.Types


scrobbler ∷ IO ()
scrobbler = withSocketsDo $ do
  sock ← socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  bindSocket sock (SockAddrInet lohPort iNADDR_ANY)
  listen sock 1024
  forever $ serve sock


serve ∷ Socket → IO ()
serve sock = do
  (s, _) ← accept sock
  h ← socketToHandle s ReadWriteMode
  hSetBuffering h LineBuffering
  void $ forkIO $ evalStateT (playerLoop h) []

playerLoop ∷ Handle → StateT [Task] IO ()
playerLoop h = do
  newTask  ← lift $ decode <$> BS.hGetContents h
  taskDone ← lift $ maybe (return False) doTask newTask
  unless taskDone $ modify (maybe id (:) newTask)
  -- handle failed tasks
  when taskDone $ do
    tasks ← get
    newTasks ← lift $ dropWhileM doTask tasks
    put newTasks
  tasks ← get
  when (length tasks > 0) $
      lift $ debugM "Scrobbler" $ "failed tasks: " ++ show (length tasks)
  playerLoop h

doTask ∷ Task → IO Bool
doTask (Task taskType config taskTrack@(Track _ ar _ t)) = do
  st ← toCommand config taskTrack
  case st of
    Right _ → do
      infoM "Scrobbler" okMessage
      return True
    Left _ → do
      warningM "Scrobbler" failMessage
      return False
  where
    toCommand = case taskType of
                  Scrobble → scrobbleTrack
                  UpdateNowPlaying → nowPlaying

    okMessage, failMessage ∷ String
    okMessage   = printf "%s \"%s - %s\"" taskName ar t
    failMessage = okMessage ++ " failed"

    taskName ∷ String
    taskName = case taskType of
                 Scrobble → "scrobble"
                 UpdateNowPlaying → "now playing"

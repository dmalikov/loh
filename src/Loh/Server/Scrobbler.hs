{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative ((<$>))
import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (handle, SomeException)
import Control.Monad (forever, void)
import Control.Monad.Loops (dropWhileM)
import Control.Monad.State
import Data.Aeson (decode)
import Data.Maybe (maybe)
import Network.Socket
import System.IO
import System.Log.Logger
import System.Log.Handler (setFormatter)
import System.Log.Handler.Simple (streamHandler)
import System.Log.Formatter (tfLogFormatter)
import Text.Printf

import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Sequence as S

import Loh.Core.LastFM.Method
import Loh.Core.Types


main ∷ IO ()
main = do
  handler ← streamHandler stderr DEBUG >>= \lh → return $
    setFormatter lh (tfLogFormatter "%F %T" "[$time] ($prio) $loggername: $msg")
  updateGlobalLogger "" (setLevel DEBUG . setHandlers [handler])
  void $ forkIO scrobbler
  forever $ threadDelay 1000000


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

playerLoop :: Handle → StateT [Task] IO ()
playerLoop h = do
  newTask ← lift $ decode <$> BS.hGetContents h
  taskDone ← lift $ maybe (return False) doTask newTask
  unless taskDone $ modify (maybe id (:) newTask)
  -- handle failed tasks
  when taskDone $ do
    tasks ← get
    newTasks ← lift $ dropWhileM doTask tasks
    put newTasks
  tasks ← get
  lift $ debugM "Scrobbler" $ "failed tasks: " ++ show tasks
  playerLoop h

doTask ∷ Task → IO Bool
doTask p = do
  st ← toCommand p (lfmConfigT p) (trackInfoT p)
  case st of
    Right _ → do
      infoM "Scrobbler" $ okMessage p
      return True
    Left _ → do
      warningM "Scrobbler" $ failMessage p
      return False
  where
    toCommand p = case typeT p of
                    Scrobble → scrobbleTrack
                    UpdateNowPlaying → nowPlaying

    okMessage p = printf "%s \"%s - %s\"" taskName (artist τ) (track τ)

    failMessage p = okMessage p ++ " failed"

    taskName ∷ String
    taskName = case typeT p of
                 Scrobble → "scrobble"
                 UpdateNowPlaying → "now playing"

    τ = trackInfoT p


{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative ((<$>))
import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (handle, SomeException)
import Control.Monad (forever, void)
import Data.Aeson (decode)
import Data.Maybe (maybe)
import Network.Socket
import System.IO
import System.Log.Logger
import System.Log.Handler (setFormatter)
import System.Log.Handler.Simple (streamHandler)
import System.Log.Formatter (tfLogFormatter)
import Text.Printf

import Data.ByteString.Lazy.Char8 as BS

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
  void $ forkIO $ playerLoop h

playerLoop :: Handle -> IO ()
playerLoop h = do
  maybe (return ()) doTask =<< decode <$> BS.hGetContents h
  playerLoop h

doTask ∷ Packet → IO ()
doTask ρ = do
  st ← toCommand ρ (lfmConfigP ρ) (trackInfoP ρ)
  case st of
    Right _ → infoM "Scrobbler" $ okMessage ρ
    Left _ → warningM "Scrobbler" $ failMessage ρ
  where
    toCommand p = case taskP p of
                    Scrobble → scrobbleTrack
                    UpdateNowPlaying → nowPlaying

    okMessage ∷ Packet → String
    okMessage p = printf "%s \"%s - %s\"" taskName (artist τ) (track τ)
      where taskName ∷ String
            taskName = case taskP p of
                         Scrobble → "scrobble"
                         UpdateNowPlaying → "now playing"
            τ = trackInfoP p
    failMessage p = okMessage p ++ " failed"


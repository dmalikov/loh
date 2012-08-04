{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative ((<$>))
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever, void)
import Data.Aeson (decode)
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
  maybePacket ← decode <$> BS.hGetContents h
  case maybePacket of
    Nothing → playerLoop h
    Just ρ → do
      case taskP ρ of
        Scrobble → do
          st ← scrobbleTrack (lfmConfigP ρ) (trackInfoP ρ)
          case st of
            Right _ →
              infoM "Scrobbler" $ "Scrobbled " ++ show (trackInfoP ρ)
            Left  _ → do
              warningM "Scrobbler" $ "Scrobble failed " ++ show (trackInfoP ρ)
        UpdateNowPlaying → do
          st ← nowPlaying (lfmConfigP ρ) (trackInfoP ρ)
          case st of
            Right _ → debugM "Scrobbler" $ logNowPlaying ρ
            Left _ → warningM "Scrobbler" $ logNowPlayingFailed ρ
      playerLoop h
  where
    logNowPlaying = log_ "[%s] now playing \"%s - %s\""

    logNowPlayingFailed = log_ "[%s] now playing \"%s - %s\" failed"

    log_ format ρ = printf format (show $ playerNameP ρ) (artist τ) (track τ)
      where τ = trackInfoP ρ

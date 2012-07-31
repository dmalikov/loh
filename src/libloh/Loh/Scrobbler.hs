{-# LANGUAGE OverloadedStrings #-}
module Loh.Scrobbler
  ( scrobbler
  ) where

import Control.Applicative ((<$>))
import Control.Monad (forever, void)
import Control.Concurrent (forkIO)
import Data.Aeson (decode)
import Network.Socket
import System.IO
import System.Log.Logger (infoM, warningM)

import Data.ByteString.Lazy.Char8 as BS

import Loh.DB
import Loh.LastFM.Method
import Loh.Types


scrobbler ∷ LFMConfig → IO ()
scrobbler c = withSocketsDo $ do
  sock ← socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  bindSocket sock (SockAddrInet lohPort iNADDR_ANY)
  listen sock 1024
  forever $ serve c sock

serve ∷ LFMConfig → Socket → IO ()
serve c sock = do
  (s, _) ← accept sock
  h ← socketToHandle s ReadWriteMode
  hSetBuffering h LineBuffering
  void $ forkIO $ playerLoop c h

playerLoop :: LFMConfig → Handle -> IO ()
playerLoop c h = do
  maybeTrack ← decode <$> BS.hGetContents h
  case maybeTrack of
    Nothing → playerLoop c h
    Just τ → do
      st ← scrobbleTrack c τ
      case st of
        Right _ →
          infoM "Loh.Scrobbler" $ "Scrobbled " ++ show τ
        Left  _ → do
          warningM "Loh.Scrobbler" $ "Scrobble failed " ++ show τ
          store τ
      playerLoop c h

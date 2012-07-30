module Loh.Scrobbler where

import Control.Applicative ((<$>))
import Control.Monad (forever, void)
import Control.Concurrent (forkIO)
import Network.Socket
import System.IO

import Loh.DB
import Loh.LastFM.Method
import Loh.Log
import Loh.Types

serve ∷ LFMConfig → Socket → IO ()
serve c sock = do
  (s, _) <- accept sock
  h <- socketToHandle s ReadWriteMode
  hSetBuffering h LineBuffering
  void $ forkIO $ playerLoop c h

playerLoop :: LFMConfig → Handle -> IO ()
playerLoop c h = do
  τ ← read <$> hGetLine h
  st ← scrobbleTrack c τ
  case st of
    Right _ → logMessage $ "Scrobbled " ++ show τ
    Left  _ → do
      logMessage $ "Scrobble failed " ++ show τ
      store τ
  playerLoop c h

scrobbler ∷ LFMConfig → IO ()
scrobbler c = withSocketsDo $ do
  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  bindSocket sock (SockAddrInet 9114 iNADDR_ANY)
  listen sock 1024
  forever $ serve c sock

module Loh.Scrobbler where

import Control.Applicative ((<$>))
import Control.Monad (forever, void)
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan
import Network.Socket
import System.IO

import Loh.DB
import Loh.LastFM.Method
import Loh.Log
import Loh.Types

serve ∷ LFMConfig → Socket → Chan String → IO ()
serve c sock _ = do
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

readLoop :: Chan String -> IO ()
readLoop ch = readChan ch >>= const (return ())

scrobbler ∷ LFMConfig → IO ()
scrobbler c = withSocketsDo $ do
  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  bindSocket sock (SockAddrInet 7123 iNADDR_ANY)
  listen sock 1024
  ch <- newChan
  forkIO (forever $ readLoop ch)
  forever $ serve c sock ch

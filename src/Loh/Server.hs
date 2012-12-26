{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Concurrent         (forkIO)
import           System.IO

import           Control.Monad.State
import qualified Data.ByteString as B
import           Data.Serialize (decode)
import           Network.Socket
import           Network.Lastfm


main :: IO ()
main = scrobbler


scrobbler :: IO ()
scrobbler = withSocketsDo $ do
  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  bindSocket sock (SockAddrInet lohPort iNADDR_ANY)
  listen sock 1024
  forever $ serve sock


serve :: Socket -> IO ()
serve sock = do
  (s, _) <- accept sock
  h <- socketToHandle s ReadWriteMode
  hSetBuffering h LineBuffering
  void $ forkIO $ evalStateT (playerLoop h) []


-- | Broken
playerLoop :: Handle -> StateT [R JSON Send Ready] IO ()
playerLoop h = do
  nT <- lift $ decode <$> B.hGetContents h
  case nT of
    Left _ -> return ()
    Right newTask -> do
      taskDone <- lift $ doTask newTask
      unless taskDone $ modify $ (:) newTask
      playerLoop h


doTask :: R JSON Send Ready -> IO Bool
doTask r = lastfm' r >> return True


lohPort :: PortNumber
lohPort = 9114

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Concurrent         (forkIO)
import           System.IO

import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import           Control.Monad.State
import           Network.Socket
import           Network.Lastfm hiding (get)


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
  newTask  <- lift $ decode <$> B.hGetContents h
  taskDone <- lift $ doTask newTask
  unless taskDone $ modify $ (:) newTask
  playerLoop h


doTask :: R JSON Send Ready -> IO Bool
doTask r = lastfm' r >> return True


decode :: ByteString -> R JSON Send Ready
decode = undefined


lohPort :: PortNumber
lohPort = 9114

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent (forkIO)
import Control.Exception (catch)
import Control.Monad
import Data.IORef (IORef, newIORef, readIORef, writeIORef, atomicModifyIORef')
import System.IO

import           Control.Lens
import qualified Data.ByteString as B
import           Data.Serialize (decode)
import           Network
import qualified Network.HTTP.Conduit as C
import qualified Network.HTTP.Types as C
import           Network.Lastfm
import           Network.Lastfm.Internal


type Job = R JSON Send Ready
type Pool = IORef [Job]


main :: IO ()
main = do
  sock <- listenOn lohPort
  pool <- newIORef []
  forever $ do
    (h, _, _) <- accept sock
    forkIO $ getJob h pool
 where
  lohPort = PortNumber 9114


-- | Broken
getJob :: Handle -> Pool -> IO ()
getJob h p = do
  eej <- decode <$> B.hGetContents h
  case eej of
    Left _ -> return ()
    Right j -> unless (broken j) (atomicModifyIORef' p (\js -> (j : js, ())))


broken :: R JSON Send Ready -> Bool
broken r = view (query . _at "method") r `notElem`
  [ "track.scrobble"
  , "track.updateNowPlaying"
  , "track.love"
  ]


runJobs :: Pool -> IO ()
runJobs pref = readIORef pref >>= filterM runJob >>= writeIORef pref


runJob :: Job -> IO Bool
runJob j = do
  catch (lastfm' j >> return True) (return . badJob)
 where
  badJob :: C.HttpException -> Bool
  badJob (C.StatusCodeException s _) = C.statusIsClientError s
  badJob (C.ResponseTimeout) = False
  badJob _ = True


report :: Pool -> IO ()
report pref = do
  pool <- readIORef pref
  putStrLn $ "there are " ++ show (length pool) ++ " jobs currently pending."

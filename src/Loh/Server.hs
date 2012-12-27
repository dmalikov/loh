{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
import Control.Exception (catch)
import Control.Monad
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
type Pool = MVar [Job]


main :: IO ()
main = do
  sock <- listenOn lohPort
  pref <- newMVar []
  forkIO $ runJobs pref >> threadDelay 60000000
  forever $ do
    (h, _, _) <- accept sock
    forkIO $ getJob h pref
 where
  lohPort = PortNumber 9114


runJobs :: Pool -> IO ()
runJobs pref = report pref >> modifyMVar_ pref (filterM runJob)


report :: Pool -> IO ()
report pref = withMVar pref $ \pool ->
  putStrLn $ "there are " ++ show (length pool) ++ " jobs currently pending."


runJob :: Job -> IO Bool
runJob j = do
  catch (lastfm' j >> return True) (return . badJob)
 where
  badJob :: C.HttpException -> Bool
  badJob (C.StatusCodeException s _) = C.statusIsClientError s
  badJob (C.ResponseTimeout) = False
  badJob _ = True


getJob :: Handle -> Pool -> IO ()
getJob h pref = do
  eej <- decode <$> B.hGetContents h
  case eej of
    Left _ -> return ()
    Right j -> unless (broken j) (modifyMVar_ pref (return . (j:)))
  hClose h


broken :: R JSON Send Ready -> Bool
broken r = view (query . _at "method") r `notElem`
  [ "track.scrobble"
  , "track.updateNowPlaying"
  , "track.love"
  ]

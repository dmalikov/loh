{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
import Control.Exception (catch)
import Control.Monad
import Data.Unique
import System.IO

import           Control.Lens
import qualified Data.ByteString.Char8 as B
import           Data.IntMap (IntMap)
import qualified Data.IntMap as M
import           Data.Serialize (decode)
import           Network
import qualified Network.HTTP.Conduit as C
import qualified Network.HTTP.Types as C
import           Network.Lastfm
import           Network.Lastfm.Internal


type Job = R JSON Send Ready
type Pool = MVar (IntMap Job)


main :: IO ()
main = do
  sock <- listenOn lohPort
  pref <- newMVar M.empty
  forkIO $ forever $ runJobs pref >> threadDelay 60000000
  forever $ do
    (h, _, _) <- accept sock
    forkIO $ getJob h pref
 where
  lohPort = PortNumber 9114


runJobs :: Pool -> IO ()
runJobs pref = report pref >> modifyMVar_ pref batch
 where
  batch jobs = M.differenceWith (\j b -> if b then Just j else Nothing) jobs <$> traverse runJob jobs


report :: Pool -> IO ()
report pref = withMVar pref $ \pool ->
  putStrLn $ "there are " ++ show (M.size pool) ++ " jobs currently pending."


runJob :: Job -> IO Bool
runJob j =
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
    Right j -> unless (broken j) $ do
      unique <- hashUnique <$> newUnique
      modifyMVar_ pref (return . M.insert unique j)
      B.hPut h (B.pack $ show unique)
  hClose h


broken :: R JSON Send Ready -> Bool
broken r = view (query . _at "method") r `notElem`
  [ "track.scrobble"
  , "track.updateNowPlaying"
  , "track.love"
  ]

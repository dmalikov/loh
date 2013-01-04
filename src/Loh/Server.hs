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
  rpoo <- newMVar M.empty
  forkIO $ forever $ runJobs rpoo >> threadDelay 60000000
  forever $ do
    (h, _, _) <- accept sock
    forkIO $ getJob h rpoo
 where
  lohPort = PortNumber 9114


runJobs :: Pool -> IO ()
runJobs rpoo = report rpoo >> modifyMVar_ rpoo batch
 where
  batch jobs = M.differenceWith (\j b -> if b then Nothing else Just j) jobs <$> traverse runJob jobs


report :: Pool -> IO ()
report rpoo = withMVar rpoo $ \pool ->
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
getJob h rpoo = do
  msgSize <- decode <$> B.hGet h 8
  case msgSize of
    Left _ -> return ()
    Right size -> do
      eej <- decode <$> B.hGet h size
      case eej of
        Left _ -> return ()
        Right j -> unless (broken j) $ do
          unique <- hashUnique <$> newUnique
          B.hPut h (B.pack $ show unique)
  hClose h


broken :: R JSON Send Ready -> Bool
broken r = view (query . _at "method") r `notElem`
  [ "track.scrobble"
  , "track.updateNowPlaying"
  , "track.love"
  ]

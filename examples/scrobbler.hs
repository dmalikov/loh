{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (forever, when)
import Data.Int (Int64)
import Data.Maybe (fromMaybe)

import           Control.Monad.Trans (liftIO)
import qualified Data.Map as M
import           Data.Text (Text)
import           Data.Time (formatTime, getCurrentTime)
import qualified Network.MPD as MPD
import           Network.Lastfm
import qualified Network.Lastfm.Track as Track
import           Loh.Client (send)
import           System.Locale (defaultTimeLocale)



data TrackInfo = TrackInfo
  { _artist :: Text
  , _album :: Text
  , _track :: Text
  , _duration :: Int64
  , _timestamp :: Int64
  }


ak :: Request f a APIKey
ak = apiKey "__YOUR_API_KEY__"

sk :: Request f Sign SessionKey
sk = sessionKey "__YOUR_SESSION_KEY__"

secret :: Secret
secret = Secret "__YOUR_SECRET__"


main :: IO ()
main = forever $ eventLoop (\lti ti -> setNowPlaying ti >> scrobble lti) >> threadDelay 1000000


eventLoop :: (TrackInfo -> TrackInfo -> IO a) -> IO (MPD.Response ())
eventLoop handler = MPD.withMPD $ do
  ts <- read . formatTime defaultTimeLocale "%s" <$> liftIO getCurrentTime
  loop (MPD.defaultSong "") ts
 where
  loop ls lts = do
    liftIO $ threadDelay 1000000
    maybeSong <- MPD.currentSong
    state <- MPD.stState <$> MPD.status
    nts <- read . formatTime defaultTimeLocale "%s" <$> liftIO getCurrentTime
    when (state == MPD.Playing) $
      case maybeSong of
        Just s ->
          case () of
            _ | s /= ls || nts - lts > fromIntegral (MPD.sgLength ls) -> do
                liftIO $ handler (trackInfo ls lts) (trackInfo s nts)
                loop s nts
              | otherwise -> loop s lts
        Nothing -> loop ls lts
   where
    trackInfo song ts = TrackInfo
      { _artist = MPD.toText getArtist
      , _album = MPD.toText getAlbum
      , _track = MPD.toText getTrack
      , _duration = fromIntegral $ MPD.sgLength song
      , _timestamp = fromIntegral ts
      }
     where
      info = MPD.sgTags song
      getTrack = fromMaybe "No Title" $ head <$> M.lookup MPD.Title info
      getArtist = fromMaybe "No Artist" $ head <$> M.lookup MPD.Artist info
      getAlbum = fromMaybe "No Album" $ head <$> M.lookup MPD.Album info


setNowPlaying :: TrackInfo -> IO ()
setNowPlaying TrackInfo { _artist = ar, _track = t, _album = al, _duration = d } =
  send "localhost" 9114 . sign secret $
    Track.updateNowPlaying <*> artist ar <*> track t <* album al <* duration d <*> ak <*> sk


scrobble :: TrackInfo -> IO ()
scrobble TrackInfo { _artist = ar, _track = t, _album = al, _duration = d, _timestamp = ts } = do
  nts <- read . formatTime defaultTimeLocale "%s" <$> getCurrentTime
  when (nts - ts > fromIntegral (d `div` 2)) $
    send "localhost" 9114 . sign secret $
      Track.scrobble <*> artist ar <*> track t <*> timestamp nts <* album al <*> ak <*> sk

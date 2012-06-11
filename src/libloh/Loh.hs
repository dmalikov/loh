{-# LANGUAGE UnicodeSyntax #-}
module Main where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (forever)
import Data.Char (isSpace)
import Data.List.Split (splitOn)
import Data.Maybe
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))

import qualified Network.Lastfm as LFM

import Loh.Eventer
import Loh.Types

configFilePath ∷ FilePath
configFilePath = ".lastfm.conf"

getConfig ∷ IO LFMConfig
getConfig = do
  configContent ← readFile <$> (</> configFilePath) <$> getHomeDirectory
  values ← map ((\[x,y] -> (x,y)) . splitOn "=" . filter (not . isSpace)) . lines <$> configContent
  return $ fromMaybe (error "Config at ~/.lastfm.conf should contain APIKey, SessionKey and Secret") (readConfig values)
  where
    readConfig xs = (,,) <$> (LFM.APIKey <$> lookup "APIKey" xs) <*> (LFM.SessionKey <$> lookup "SessionKey" xs) <*> (LFM.Secret <$> lookup "Secret" xs)

main ∷ IO ()
main = forever $ eventer =<< getConfig

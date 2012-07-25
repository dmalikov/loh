{-# LANGUAGE OverloadedStrings #-}
module Loh.Config
  ( LConfig(..)
  , readConfig, writeConfig
  ) where

import Control.Applicative ((<$>), empty)
import System.IO (IOMode(ReadMode), withFile)

import Data.Aeson
import qualified Data.ByteString.Lazy as B
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))
import Network.Lastfm

import Loh.Players
import Loh.Types


data LConfig = LConfig
                 { players ∷ [Player]
                 , lfmConfig ∷ LFMConfig
                 }


instance FromJSON LConfig where
  parseJSON (Object o) =
    do players' ← map (fromName . (read ∷ String → PlayerName)) <$> o .: "players"
       apiKey ← o .: "APIKey"
       sessionKey ← o .: "SessionKey"
       secret ← o .: "Secret"
       return LConfig
                { players = players'
                , lfmConfig = (APIKey apiKey, SessionKey sessionKey, Secret secret)
                }
  parseJSON _ = empty


instance ToJSON LConfig where
  toJSON (LConfig ps (APIKey ak, SessionKey sk, Secret s)) =
    object ["players" .= map (show . name) ps, "APIKey" .= ak, "SessionKey" .= sk, "Secret" .= s]


readConfig ∷ IO LConfig
readConfig =
  do hd ← getHomeDirectory
     withFile (hd </> configFileName) ReadMode $ \h →
       do contents ← B.hGetContents h
          case decode contents of
            Just config → return config
            Nothing → fail "Malformed configuration file"


writeConfig ∷ LConfig → IO ()
writeConfig config =
  do hd ← getHomeDirectory
     B.writeFile (hd </> configFileName) (encode config)


configFileName ∷ String
configFileName = ".lohrc"

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Loh.Client.Config
  ( LConfig(..)
  , readConfig, writeConfig
  ) where

import Control.Applicative ((<$>), empty)
import Data.Aeson hiding (encode)
import Data.List (intersperse)
import Data.Monoid ((<>), mconcat)
import Data.Text (Text)
import Data.Text.Lazy.Builder (Builder, toLazyText)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Network.Lastfm hiding (Value)
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))
import System.IO (IOMode(ReadMode), withFile)

import qualified Data.Aeson.Encode as A (fromValue)
import qualified Data.ByteString.Lazy as B
import qualified Data.HashMap.Strict as H (toList)
import qualified Data.Vector as V (toList)

import Loh.Core.Players
import Loh.Core.Types


data LConfig = LConfig
  { players ∷ [Player]
  , lfmConfig ∷ LFMConfig
  , serverHost ∷ String
  }

instance FromJSON LConfig where
  parseJSON (Object o) = do
    players' ← map (fromName . (read ∷ String → PlayerName)) <$> o .: "players"
    apiKey ← o .: "APIKey"
    sessionKey ← o .: "SessionKey"
    secret ← o .: "Secret"
    serverHost' ← o .: "ServerHost"
    return LConfig
      { players = players'
      , lfmConfig = LFMConfig (APIKey apiKey) (SessionKey sessionKey) (Secret secret)
      , serverHost = serverHost'
      }
  parseJSON _ = empty

instance ToJSON LConfig where
  toJSON (LConfig ps (LFMConfig (APIKey ak) (SessionKey sk) (Secret s)) sh) =
    object [ "players" .= map (show . name) ps
           , "APIKey" .= ak
           , "SessionKey" .= sk
           , "Secret" .= s
           , "ServerHost" .= sh
           ]


readConfig ∷ IO (Maybe LConfig)
readConfig = do
  hd ← getHomeDirectory
  withFile (hd </> configFileName) ReadMode $ \h → do
    !contents ← B.hGetContents h
    return $ decode contents

writeConfig ∷ LConfig → IO ()
writeConfig config = do
  hd ← getHomeDirectory
  B.writeFile (hd </> configFileName) (encode config)
 where
  encode = encodeUtf8 . toLazyText . fromValue 0 . toJSON


configFileName ∷ String
configFileName = ".lohrc"

fromValue ∷ Int → Value → Builder
fromValue indent (Array v) =
  fromComplex indent ("[","]") fromValue (V.toList v)
fromValue indent (Object m) =
  fromComplex indent ("{","}") fromKeyValue (H.toList m)
fromValue _ v = A.fromValue v

fromComplex ∷ Int → (Builder, Builder) → (Int → a → Builder) → [a] → Builder
fromComplex _ (delimL,delimR) _ [] = delimL <> delimR
fromComplex indent (delimL,delimR) fromItem items =
  mconcat [delimL, "\n", items', "\n", fromIndent indent, delimR]
 where
  items' = mconcat . intersperse ",\n" $
    map (\item → fromIndent (succ indent) <> fromItem (succ indent) item) items

fromKeyValue ∷ Int → (Text, Value) → Builder
fromKeyValue indent (k,v) =
  A.fromValue (toJSON k) <> ": " <> fromValue indent v

fromIndent ∷ Int → Builder
fromIndent = mconcat . flip replicate "  "

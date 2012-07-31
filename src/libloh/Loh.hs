{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Applicative ((<$>))
import Control.Exception (SomeException, catch)
import Network.Lastfm (APIKey(..), Secret(..))
import Network.Lastfm.JSON.Auth
import Prelude hiding (catch)
import System.IO (stderr)
import System.Log.Logger (Priority(DEBUG), updateGlobalLogger, setLevel, setHandlers)
import System.Log.Handler (setFormatter)
import System.Log.Handler.Simple (streamHandler)
import System.Log.Formatter (tfLogFormatter)

import Loh.Config (LConfig(..), readConfig, writeConfig)
import Loh.Eventer


main ∷ IO ()
main = do
  decodedConfig ← catch readConfig $ \(_ ∷ SomeException) → genConfigSkeleton
  case decodedConfig of
    Just config → do
      handler ← streamHandler stderr DEBUG >>= \lh → return $
        setFormatter lh (tfLogFormatter "%F %T" "[$time] ($prio) $loggername: $msg")
      updateGlobalLogger "" (setLevel INFO . setHandlers [handler])
      eventer config
    Nothing → error "Malformed ~/.lohrc"


genConfigSkeleton ∷ IO (Maybe LConfig)
genConfigSkeleton = do
  token ← extract <$> getToken apiKey
  putStrLn $ unlines
    [ "~/.lohrc seems to be missing"
    , "You need correct session key to use loh properly."
    , "Authorize your token: " ++ getAuthorizeTokenLink apiKey token
    , "And then press any key."
    ]
  getChar
  sk ← extract <$> getSession apiKey token secret
  writeConfig LConfig
    { players = []
    , lfmConfig = (apiKey, sk, secret)
    }
  putStrLn $ unlines
    [ "Session key is successfuly written to ~/.lohrc"
    , "Add players you wish to scrobble from and restart loh."
    ]
  return Nothing
 where
  apiKey = APIKey "34a538d1ce307a257c695bcc7e031392"
  secret = Secret "4bd6283d6441cdf842d03ba8ab7a6ddf"

  extract (Left e) = error $ show e
  extract (Right v) = v

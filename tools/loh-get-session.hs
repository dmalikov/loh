{-# LANGUAGE UnicodeSyntax #-}

import Control.Applicative ((<$>))
import Control.Arrow ((|||))
import Network.Lastfm
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))
import Text.Printf (printf)
import qualified Network.Lastfm.XML.Auth as Auth

import Loh.Config (LConfig(..), writeConfig)


ak = "34a538d1ce307a257c695bcc7e031392"
s  = "4bd6283d6441cdf842d03ba8ab7a6ddf"

parseArgs xs
  | not $ null xs = error "Usage: ./loh-get-session.hs"
  | otherwise = xs

main ∷ IO ()
main = do
  let apiKey = APIKey ak
      secret = Secret s
  token ← getToken <$> Auth.getToken apiKey
  putStrLn $ "Authorize your token: " ++ Auth.getAuthorizeTokenLink apiKey token
  _ ← getChar
  sk ← getSession <$> Auth.getSession apiKey token secret
  writeConfig LConfig { players = [], lfmConfig = (APIKey ak, SessionKey sk, Secret s) }
  putStrLn $ "Session key is successfuly written to ~/.lohrc"
    where
      getToken = error . show ||| id
      getSession = error . show ||| (\(SessionKey t) → t)

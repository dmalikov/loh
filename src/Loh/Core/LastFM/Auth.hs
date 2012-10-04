{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Loh.Core.LastFM.Auth where

import           Control.Applicative (empty, (<$>), (<*>))
import           Data.Aeson

import qualified Network.Lastfm      as LFM

type LFMConfig = (LFM.APIKey, LFM.SessionKey, LFM.Secret)

instance FromJSON LFM.APIKey where
  parseJSON (Object o) = LFM.APIKey <$> o .: "apiKey"
  parseJSON _ = empty

instance ToJSON LFM.APIKey where
  toJSON (LFM.APIKey ak) = object [ "apiKey" .= ak ]

instance ToJSON LFM.SessionKey where
  toJSON (LFM.SessionKey sk) = object [ "session" .= object [ "key" .= sk ] ]

instance FromJSON LFM.Secret where
  parseJSON (Object o) = LFM.Secret <$> o .: "secret"
  parseJSON _ = empty

instance ToJSON LFM.Secret where
  toJSON (LFM.Secret s) = object [ "secret" .= s ]

instance FromJSON LFMConfig where
  parseJSON (Object o) = (,,) <$>
     LFM.APIKey     <$> o .: "apiKey" <*>
    (LFM.SessionKey <$> o .: "sessionKey") <*>
    (LFM.Secret     <$> o .: "secret")
  parseJSON _ = empty

instance ToJSON LFMConfig where
  toJSON (LFM.APIKey ak, LFM.SessionKey sk, LFM.Secret s) = object
    [ "apiKey"     .= ak
    , "sessionKey" .= sk
    , "secret"     .= s
    ]


{-# LANGUAGE DataKinds #-}
module Loh.Client (send, LohClientException(..)) where

import Network.Lastfm


data LohClientException =
    LohServerNotFound
  | LohEncryptionIsBroken
  | LohRequestIsBroken
  | LohUnknownException


-- | Send liblastfm request to Loh.
--
-- If request is accepted when there would be no response.
-- Otherwise it would throw on of exceptions defined here
send :: Request f Send Ready -- ^ Request to send
     -> String               -- ^ Loh hostname
     -> Int                  -- ^ Loh port
     -> IO ()
send = undefined

{-# LANGUAGE DataKinds #-}
module Loh.Client (send, LohClientException(..)) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import           Network.Lastfm
import           Network


data LohClientException =
    LohServerNotFound
  | LohEncryptionIsBroken
  | LohRequestIsDefinitelyBroken
  | LohRequestIsBroken
  | LohUnknownException


-- | Send liblastfm request to Loh.
--
-- If request is accepted when there would be no response.
-- Otherwise it would throw on of exceptions defined here
send :: Request JSON Send Ready -- ^ Request to send
     -> String                  -- ^ Loh hostname
     -> Int                     -- ^ Loh port
     -> IO ()
send r h p = do
  q <- connectTo h (PortNumber $ fromIntegral p)
  B.hPut q (encode (finalize r))


encode :: R JSON Send Ready -> ByteString
encode = undefined

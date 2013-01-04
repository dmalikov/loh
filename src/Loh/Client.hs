{-# LANGUAGE DataKinds #-}
module Loh.Client (send, LohClientException(..)) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BSC
import           Data.Serialize (encode)
import           Network.Lastfm
import           Network
import           System.IO (hClose)


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
send :: String                  -- ^ Loh hostname
     -> Int                     -- ^ Loh port
     -> Request JSON Send Ready -- ^ Request to send
     -> IO (Maybe Int)          -- ^ Job ID
send host port r = do
  h <- connectTo host (PortNumber $ fromIntegral port)
  B.hPut h $ encode $ B.length msg
  B.hPut h msg
  maybeID <- BSC.readInt <$> B.hGet h 8
  hClose h
  return $ fst <$> maybeID
 where
  msg = encode $ finalize r

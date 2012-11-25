module Main where

import           Control.Concurrent         (forkIO)
import           Control.Monad              (forM_, void)
import           Data.Aeson                 (encode)
import qualified Data.ByteString.Lazy.Char8 as BS
import           Network
import           System.IO

import           Loh.Client.Config          (LConfig (..), readConfig)
import           Loh.Core.Task
import           Loh.Core.Types

main ∷ IO ()
main = do
  maybeConfig ← readConfig
  case maybeConfig of
    Just config → do
      forM_ (players config) $ \ρ → withSocketsDo $ do
        h ← connectTo (serverHost config) (PortNumber lohPort)
        hSetBuffering h LineBuffering
        void . forkIO $ do
          maybeTrack ← getInfo ρ
          case maybeTrack of
            Just τ → BS.hPut h $ encode $ Task Love (lfmConfig config) $ toTrack τ
            _ → return ()
    _ → return ()

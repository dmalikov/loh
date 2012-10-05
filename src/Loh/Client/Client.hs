{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Control.Exception         (SomeException, catch)
import           System.IO                 (stderr)
import           System.Log.Formatter      (tfLogFormatter)
import           System.Log.Handler        (setFormatter)
import           System.Log.Handler.Simple (streamHandler)
import           System.Log.Logger         (Priority (DEBUG, INFO), setHandlers,
                                            setLevel, updateGlobalLogger)

import           Loh.Client.Config         (genConfigSkeleton, readConfig)
import           Loh.Client.Eventer


debugLevel ∷ Priority
#ifdef __DEBUG__
debugLevel = DEBUG
#else
debugLevel = INFO
#endif

main ∷ IO ()
main = do
  decodedConfig ← catch readConfig $ \(_ ∷ SomeException) → genConfigSkeleton
  case decodedConfig of
    Just config → do
      handler ← streamHandler stderr DEBUG >>= \lh → return $
        setFormatter lh (tfLogFormatter "%F %T" "[$time] ($prio) $loggername: $msg")
      updateGlobalLogger "" (setLevel debugLevel . setHandlers [handler])
      eventer config
    Nothing → error "Malformed ~/.lohrc"

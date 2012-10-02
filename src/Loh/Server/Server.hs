module Main where

import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (handle, SomeException(..))
import Control.Monad (forever, join, liftM2, void, when)
import Options.Applicative
import System.Directory (createDirectory, getHomeDirectory)
import System.FilePath.Posix ((</>))
import System.IO (stderr)
import System.Log.Logger
import System.Log.Handler (setFormatter)
import System.Log.Handler.Simple (streamHandler)
import System.Log.Formatter (tfLogFormatter)
import System.Posix.Daemonize
import System.Posix.Files (fileExist, removeLink)
import System.Posix.Process (getProcessID)
import System.Posix.Signals (signalProcess)
import System.Posix.Types
import Text.Read (readMaybe)

import Loh.Server.Scrobbler

data Configuration = Configuration
  { mode ∷ Mode
  , needToKill ∷ Bool
  }

data Mode = Foreground | Background

optParser ∷ Parser Configuration
optParser = Configuration
  <$> flag Background Foreground
      ( long "foreground"
      & short 'f'
      & help "Run lohd in a foreground"
      )
  <*> flag False True
      ( long "kill"
      & short 'k'
      & help "Kill lohd process"
      )


main ∷ IO ()
main = run =<< execParser opts
  where opts = info (helper <*> optParser)
                ( briefDesc
                & progDesc "Loh daemon")

run ∷ Configuration → IO ()
run (Configuration _ True) = printStatus =<< kill
  where printStatus ∷ Bool → IO ()
        printStatus s = do
          if s then putStrLn "Lohd successfully killed"
               else putStrLn "Lohd is not running, there is nothing to kill"
run (Configuration mode _) = do
  createLohdDir =<< lohdDirNotExist
  case mode of
    Foreground → process
    Background → pidExist >>= \exist →
        if exist then putStrLn "Warning: lohd is already running!"
                 else daemonize $ pidWrite >> process

process ∷ IO ()
process = do
  handler ← streamHandler stderr DEBUG >>= \lh → return $
    setFormatter lh $ tfLogFormatter "%F %T" "[$time] ($prio) $loggername: $msg"
  updateGlobalLogger "" (setLevel DEBUG . setHandlers [handler])
  void $ forkIO scrobbler
  forever $ threadDelay 1000000

lohdDirPath ∷ IO FilePath
lohdDirPath = (</> ".lohd") <$> getHomeDirectory

lohdDirNotExist ∷ IO Bool
lohdDirNotExist = not <$> (fileExist =<< lohdDirPath)

createLohdDir ∷ Bool → IO ()
createLohdDir needed = when needed (createDirectory =<< lohdDirPath)

pidPath ∷ IO FilePath
pidPath = (</> "loh.pid") <$> lohdDirPath

pidRead ∷ IO (Maybe ProcessID)
pidRead = handle (\(SomeException _) → return Nothing) (readMaybe <$> (readFile =<< pidPath))

pidExist ∷ IO Bool
pidExist = fileExist =<< pidPath

pidWrite ∷ IO ()
pidWrite = bind2 writeFile pidPath (show <$> getProcessID)
  where bind2 f a b = join $ liftM2 f a b

kill ∷ IO Bool
kill = do
  maybePid ← pidRead
  case maybePid of
    Just p → do
      signalProcess 9 p
      removeLink =<< pidPath
      return True
    Nothing → return False


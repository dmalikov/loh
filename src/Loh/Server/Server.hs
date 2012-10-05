module Main where

import           Control.Concurrent        (forkIO, threadDelay)
import           Control.Exception         (SomeException (..), handle)
import           Control.Exception.Base    (try)
import           Control.Monad             (forever, join, liftM2, void, when)
import           Foreign.C.Types           (CInt (..))
import           GHC.IO.Handle.Types       (Handle (..))
import           Options.Applicative
import           System.Directory          (createDirectory, getHomeDirectory)
import           System.FilePath.Posix     ((</>))
import           System.IO                 (stderr)
import           System.Log.Formatter      (tfLogFormatter)
import           System.Log.Handler        (LogHandler (..), setFormatter)
import           System.Log.Handler.Simple (GenericHandler (..), fileHandler,
                                            streamHandler)
import           System.Log.Logger         (Priority (..), setHandlers,
                                            setLevel, updateGlobalLogger)
import           System.Posix.Daemonize
import           System.Posix.Files        (fileExist)
import           System.Posix.Process      (getProcessID)
import           System.Posix.Signals      (nullSignal, sigKILL, signalProcess)
import           System.Posix.Types
import           Text.Read                 (readMaybe)

import           Loh.Server.Scrobbler

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
        printStatus s =
          if s then putStrLn "Lohd successfully killed"
               else putStrLn "Lohd is not running, there is nothing to kill"
run (Configuration m _) = do
  createLohdDir =<< lohdDirNotExist
  case m of
    Foreground → do
      stderrHandler' ← stderrHandler
      logFileHandler' ← logFileHandler
      process [stderrHandler', logFileHandler']
    Background → pidAlive >>= \alive →
      if alive then putStrLn "Warning: lohd is already running!"
               else daemonize $ do
                 pidWrite
                 stderrHandler' ← stderrHandler
                 process [stderrHandler']
  where
    stderrHandler = do
      lh ← streamHandler stderr DEBUG
      return $ setFormatter lh $
        tfLogFormatter logFormatTimestamp logFormatMessage

    logFileHandler = do
      f ← lohdLogFilename
      lh ← fileHandler f DEBUG
      return $ setFormatter lh $
        tfLogFormatter logFormatTimestamp logFormatMessage

    logFormatTimestamp = "%F %T"
    logFormatMessage   = "[$time] ($prio) $loggername: $msg"

process ∷ [GenericHandler Handle] → IO ()
process handlers = do
  updateGlobalLogger "" (System.Log.Logger.setLevel DEBUG . setHandlers handlers)
  void $ forkIO scrobbler
  forever $ threadDelay 1000000

lohdDirPath ∷ IO FilePath
lohdDirPath = (</> ".lohd") <$> getHomeDirectory

lohdDirNotExist ∷ IO Bool
lohdDirNotExist = not <$> (fileExist =<< lohdDirPath)

createLohdDir ∷ Bool → IO ()
createLohdDir needed = when needed (createDirectory =<< lohdDirPath)

pidPath ∷ IO FilePath
pidPath = (</> "lohd.pid") <$> lohdDirPath

lohdLogFilename ∷ IO FilePath
lohdLogFilename = (</> "lohd.log") <$> lohdDirPath

pidRead ∷ IO (Maybe ProcessID)
pidRead = handle' $ readMaybe `fmap` (readFile =<< pidPath)
  where
    handle' = Control.Exception.handle $ \(SomeException _) → return Nothing

pidWrite ∷ IO ()
pidWrite = bind2 writeFile pidPath (show <$> getProcessID)
  where bind2 f a b = join $ liftM2 f a b

pidAlive ∷ IO Bool
pidAlive = signalAndHandle nullSignal

kill ∷ IO Bool
kill = signalAndHandle sigKILL

signalAndHandle ∷ CInt → IO Bool
signalAndHandle s = maybe (return False) send =<< pidRead
  where
    send pid = either (const False) (const True) <$>
      (try (signalProcess s pid) :: IO (Either IOError ()))

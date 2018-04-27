module Run where

-- * Run a node's executable.

import qualified System.Environment as Env
import           System.Process

import           Network            (Exe (..))

-- | Run the executable specified in the environment.
run :: IO ()
run = do
  exeString <- Env.getEnv "DISCODOCKERLAUNCH"
  let exe = read exeString :: Exe
  runExe exe

-- | Run the given executable.
runExe :: Exe -> IO ()
runExe exe = do
  case exe of
    Git url exe' -> do
      putStrLn "INFO: Git executables not supported"
      clone <- readProcess ("git clone " ++ url) [] []
      print clone
    Hackage _ _ -> putStrLn "INFO: Hackage executables not supported"
    Default -> do
      putStrLn "INFO: Launching default executable"
      runDefault

-- | Run an informative example as a default.
runDefault :: IO ()
runDefault = do
  putStrLn "Node finished running"

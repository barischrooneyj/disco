module Run where

-- * Run a node's executable.

-- | We want to separate out anything not related to the current service. This
-- service-independent code will be disco-boot and should be launched by the
-- boot program directly and passed any service-specific functions.

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
runExe exe =
  case exe of
    Git url exe' -> do
      putStrLn "NOTE: Git executables not supported"
      clone <- readProcess ("git clone " ++ url) [] []
      print clone
    Hackage _ _ -> putStrLn "NOTE: Hackage executables not supported"
    ServiceDefault -> do
      putStrLn "NOTE: Launching default executable"
      runServiceDefault
    EAlgorithm alg -> putStrLn $ "NOTE: We cannot run algorithm:" ++ show alg

-- | Run an informative example as a default.
runServiceDefault :: IO ()
runServiceDefault =
  putStrLn "Node finished running"

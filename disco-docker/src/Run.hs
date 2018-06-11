module Run where

-- * Run a node's executable.

-- | We want to separate out anything not related to the current service. This
-- service-independent code will be disco-boot and should be launched by the
-- boot program directly and passed any service-specific functions.

import qualified System.Environment as Env
import           System.Process

import           Network            (Program (..))

-- | Run the program specified in the environment.
run :: IO ()
run = runProgram =<< read <$> Env.getEnv "DISCO_PROGRAM"

-- | Run the given executable.
runProgram :: Program -> IO ()
runProgram program =
  case program of
    Git url program' -> do
      putStrLn "NOTE: Git executables not supported"
      clone <- readProcess ("git clone " ++ url) [] []
      print clone
    Hackage _ _ -> putStrLn "NOTE: Hackage executables not supported"
    ServiceDefault -> do
      putStrLn "NOTE: Launching default executable"
      runServiceDefault
    Algorithm alg -> putStrLn $ "NOTE: We cannot run algorithm:" ++ show alg

-- | Run an informative example as a default.
-- Useful to override this function for developing/debugging.
runServiceDefault :: IO ()
runServiceDefault =
  putStrLn "Node finished running"

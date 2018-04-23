module Lib where

import qualified System.Environment as Env
import           Network (Exe)

someFunc :: IO ()
someFunc = do
  exeString <- Env.getEnv "DISCODOCKERLAUNCH"
  let exe = read exeString :: Exe
  print exe

module Lib where

import           System.Environment (getEnvironment)

someFunc :: IO ()
someFunc = getEnvironment >>= print

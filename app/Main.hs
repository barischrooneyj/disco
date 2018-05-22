module Main where

import           Network (helloWorldNetwork)
import           Start   (startNetwork)

-- * Start the example network.

main :: IO ()
main = startNetwork helloWorldNetwork

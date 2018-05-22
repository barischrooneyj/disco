module Main where

import           HelloWorld (helloWorldNetwork)
import           Start      (startNetwork)

-- * Start the example network.

main :: IO ()
main = startNetwork helloWorldNetwork

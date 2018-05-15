module Main where

import           Network (exampleNetwork)
import           Start   (startNetwork)

-- * Start the example network.

main :: IO ()
main = startNetwork exampleNetwork

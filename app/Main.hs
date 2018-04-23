module Main where

import           Launch  (launchNetwork)
import           Network (exampleNetwork)

-- * Launch the example network.

main :: IO ()
main = launchNetwork exampleNetwork

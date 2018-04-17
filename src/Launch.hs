module Launch where

import           Data.Set       (Set)
import qualified Data.Set       as Set
import           System.Process

import           Network        (Launch (..), Messaging, Network (..),
                                 Node (..))

-- * Launch nodes and networks.

-- | Launch the given network!
launchNetwork :: Network -> IO ()
launchNetwork network = do
  print "NOTE: Only complete tautology implemented"
  launchCompleteTautology $ _nodes network

-- | Launch a complete (fully connected) network topology of nodes.
launchCompleteTautology :: Set Node -> IO ()
launchCompleteTautology nodes = do
  mapM launchNode $ Set.toList nodes
  print "Launched complete tautology!"

-- | Launch a node into the sky!
launchNode :: Node -> IO ()
launchNode node = do
  case _launch node of
    Docker -> print "foo"

-- ** Docker

-- | Creating a node.
-- --
-- -- The node needs to know which message passing type to use, which we will
-- -- include in Disco. The node will be passed the information on which message
-- -- passing system to use.

  -- handle <- runCommand $ "docker-compose up --scale disco=" ++ show times
  -- putStrLn "done"

-- -- | Constructor for a node that runs in a docker container.
-- dockerNode nid main = Node {
--   nid = nid, main = main, launch = Launch { image = dockerImage, up = dockerUp } }

-- -- | To image a Docker container we first build the project including the node's
-- -- main. Then we image a container that will run the executable after launch.
-- dockerImage :: Node -> IO FilePath
-- dockerImage node = do
--   let launchFile = "src/Launch.hs"
--   launchHs <- readFile launchFile
--   let newLaunchHs = String.replace from to launchHs
--       from = "_launchNode = putStrLn \"launch not set\""
--       to = "_launchNode = " ++ main node
--   putStrLn $ launchHs ++ "\n\n" ++ newLaunchHs

-- -- | NOTE: don't set. Over-ridden when launching a node.
-- _launchNode :: IO ()
-- _launchNode = putStrLn "launch not set"

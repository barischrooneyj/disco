module Launch where

-- * Methods of launching networks and nodes.

import           Data.Maybe       (fromJust)
import qualified System.Directory as Dir
import           System.FilePath  ((</>))
-- import           System.Process

import           Network          (Edges (..), Launch (..), Network (..),
                                   Node (..))

-- | Launch the given network!
launchNetwork :: Network -> IO ()
launchNetwork network' = do
  let network = nodesHaveUniqueIDs network'
  case _edges network of
    CompleteGraph  -> launchCompleteGraph $ _nodes network
    Edges setEdges -> print "NOTE: Only complete graph implemented"

-- | The network but all nodes have unique IDs, maybe.
--
-- The IDs of nodes that have them will not be altered, if there is a duplicate
-- among them we return Nothing.
nodesHaveUniqueIDs :: Network -> Network
nodesHaveUniqueIDs network =
  let newNodes = map (\(i, n) -> n { _id = Just i }) nodesZip
      nodesZip = zip [1..] $ _nodes network
  in network { _nodes = newNodes}

-- | Assign IDs to each node, if they don't have one.
dockerfileHead = unlines ["version: '3'", "", "services:"]

-- | Launch a complete (fully connected) network topology of nodes.
launchCompleteGraph :: [Node] -> IO ()
launchCompleteGraph nodes = do
  let dockerfile = foldl
        (\df n -> dockerfileWithNode n df) dockerfileHead nodes
  outFile <- (</> "cs/disco/docker-compose.yml") <$> Dir.getHomeDirectory
  writeFile outFile dockerfile
  print "Launched complete tautology!"

-- | A Dockerfile string with a node added.
--
-- We expect the node to have an ID.
dockerfileWithNode :: Node -> String -> String
dockerfileWithNode node dockerfile =
  case _launch node of
    Docker -> dockerfile ++ unlines [
      "  disco-docker-" ++ show (fromJust $ _id node) ++ ":",
      "    image: disco-docker",
      "    environment:",
      "      - DISCODOCKERLAUNCH=" ++ show (_exe node),
      "    command: /usr/local/bin/disco-docker-exe",
      "    tty: true"
      ]

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

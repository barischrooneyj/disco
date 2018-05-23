module Start where

-- * Start networks and nodes on services.

import           Data.Maybe           (fromJust)
import qualified System.Directory     as Dir
import           System.Exit          (ExitCode (..))
import           System.FilePath      ((</>))
import qualified System.Process.Typed as Proc

import           Network              (Edges (..), Network (..), Node (..),
                                       Service (..))

-- | Start the given network!
startNetwork :: Network -> IO ()
startNetwork network =
  case _edges network of
    CompleteGraph   -> startCompleteGraph $ _nodes network
    -- | Old Docker Compose version.
    -- CompleteGraph   -> startCompleteGraphOld $ _nodes network
    UndirectedRing  -> print "NOTE: Undirected ring not implemented"
    Edges _setEdges -> print "NOTE: Set of edges not implemented"

-- | Start a complete graph.
--
-- Currently this is done by starting all nodes on the service of the first
-- node. This works because we know we have one service currently. When we have
-- multiple services implemented we will ask each service to start the
-- respective set of nodes, at this point our messaging functionality must be
-- able to handle service boundaries.
startCompleteGraph :: [Node] -> IO ()
startCompleteGraph []          = print "No nodes to start complete graph"
startCompleteGraph nodes@(n:_) = _startNodes (_service n) nodes

-- ** Old Docker Compose code.

-- | Assign IDs to each node, if they don't have one.
dockerfileHead :: String
dockerfileHead = unlines ["version: '3'", "", "services:"]

-- | Launch a complete (fully connected) network topology of nodes.
startCompleteGraphOld :: [Node] -> IO ()
startCompleteGraphOld nodes = do
  let dockerfile = foldl dockerfileWithNode dockerfileHead nodes
  outFile <- (</> "cs/disco/docker-compose.yml") <$> Dir.getHomeDirectory
  writeFile outFile dockerfile
  print $ "Saved Dockerfile to: " ++ outFile
  (e, stdout, stderr) <- Proc.readProcess $
    Proc.proc "docker-compose" ["up"]
  print $ case e of { ExitSuccess -> stdout; ExitFailure _ -> stderr }

-- | A Dockerfile string with a node added.
dockerfileWithNode :: String -> Node -> String
dockerfileWithNode dockerfile node =
  dockerfile ++ unlines [
      "  disco-docker-" ++ show (fromJust $ _id node) ++ ":",
      "    image: disco-docker",
      "    environment:",
      "      - DISCODOCKERLAUNCH=" ++ show (_exe node),
      "    command: /usr/local/bin/disco-docker-exe",
      "    tty: true"
      ]

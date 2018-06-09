module Start where

-- * Start networks and nodes on services.
--
-- NOTE: This code is hacky and will be changed soon, don't bother investing
-- much time here.

import           Data.ByteString.Lazy.Char8 (unpack)
import           Data.Maybe                 (fromJust)
import qualified Data.Set                   as Set
import qualified System.Directory           as Dir
import           System.Exit                (ExitCode (..))
import           System.FilePath            ((</>))
import qualified System.Process.Typed       as Proc

import           Network                    (Edge (..), Edges (..),
                                             Network (..), Node (..), NodeId,
                                             Service (..))

-- | Start the given network!
startNetwork :: Network -> IO ()
startNetwork network =
  case _edges $ toStandardEdges network of
    CompleteGraph   -> startCompleteGraphOld $ _nodes network
    Edges _setEdges -> print "NOTE: Set of edges not implemented"
    _               -> print "NOTE: This is not a standard network"

-- | A network in standard format does NOT use a shorthand for edges.
toStandardEdges :: Network -> Network
toStandardEdges network =
  case _edges network of
    UndirectedRing ->
      let maxId = length $ _nodes network
          edges = foldl
            (\es n -> undirectedEdges (fromJust $ _id n) maxId ++ es)
            []
            (_nodes network)
      in network { _edges = Edges edges }
    -- | Otherwise already in standard format.
    _ -> network

-- | The edges for a node in an undirected ring.
undirectedEdges :: NodeId -> NodeId -> [Edge]
undirectedEdges nId maxId
  | nId == 1     = [Edge nId maxId    , Edge nId $ nId + 1]
  | nId == maxId = [Edge nId $ nId - 1, Edge nId 1        ]
  | otherwise    = [Edge nId $ nId - 1, Edge nId $ nId + 1]

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

-- ** OLD!! Docker Compose code.

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
  putStrLn $ unpack $ case e of { ExitSuccess -> stdout; ExitFailure _ -> stderr }

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

-- | Service as a typeclass? ------------------------------------
data LocalDocker

class Service s where
  _runNodes :: s -> [Node] -> IO ()

instance Start.Service LocalDocker where
  _runNodes = const startCompleteGraphOld
-- | ------------------------------------------------------------

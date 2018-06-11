{-# LANGUAGE DuplicateRecordFields #-}

module Network where

-- * A model of a network and its nodes.

import           Algorithm  (Algorithm)
import           Data.Maybe (mapMaybe)
import qualified Data.Set   as Set

-- | A network consists simply of nodes and edges.
data Network = Network { _nodes :: [Node], _edges :: Edges }

-- | A smart network constructor where nodes without identifiers are assigned
-- unique identifiers. Also if any given nodes have duplicate identifiers this
-- function will fail.
network :: Edges -> [Node] -> Maybe Network
network edges nodes = do
  let existingIds  = mapMaybe _identifier nodes
      newIds       = filter (`notElem` existingIds) [1..]
      nodesWithIds = zipWith (\n nId -> n { _identifier = Just nId }) nodes newIds
  if   Set.size (Set.fromList existingIds) == length existingIds
  then pure Network { _nodes = nodesWithIds, _edges = edges }
  else fail "Duplicate node identifiers in network"

-- | A simple type to compare nodes by.
type NodeId = Int

-- | A node is a program in a network.
data Node = Node {
    -- | The program to run.
    _program    :: Program
    -- | Where to run the node.
  , _service    :: Service
    -- | A unique identifier, can be set later by Disco.
  , _identifier :: Maybe NodeId
    -- | Whether the node has access to its identifier.
  , _anonymous  :: Bool
  }

-- | A node constructor with useful defaults.
node :: Service -> Program -> Node
node service exe = Node {
  _program = exe, _service = service, _identifier = Nothing, _anonymous = False }

-- | Edges define the network topology and thus the communication channels.
data Edges =
    -- | Explicit edges between pairs of nodes.
    Edges [Edge]
    -- | Alternatively a shorthand may be specified.
  | EdgesShorthand EdgesShorthand

-- | Shorthand definitions for network topologies.
data EdgesShorthand =
    -- | Every pair of nodes can communicate directly.
    CompleteGraph
    -- | Every node can communicate with ID+1 and ID-1 (wrapping around).
  | UndirectedRing

-- | An edge is a directed channel from one node to another.
data Edge = Edge { _from :: NodeId, _to :: NodeId }

-- | A service is capable of running nodes.
newtype Service = Service { _startNodes :: [Node] -> IO () }

-- | Description of a program that a service can run.
data Program =
  -- | Download and install an executable from a git URL.
    Git { _url :: String, _exe :: String }
  -- | Download and install an executable from Hackage.
  | Hackage { _package :: String, _exe :: String }
  -- | Run a service-specific default program.
  | ServiceDefault
  -- | Run the given distributed algorithm.
  | Algorithm Algorithm
  deriving (Read, Show)


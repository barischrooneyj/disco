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
  let existingIds  = mapMaybe _id nodes
      newIds       = filter (`notElem` existingIds) [1..]
      nodesWithIds = zipWith (\n nId -> n { _id = Just nId }) nodes newIds
  if   Set.size (Set.fromList existingIds) == length existingIds
  then pure Network { _nodes = nodesWithIds, _edges = edges }
  else fail "Duplicate node identifiers in network"

-- | A simple type to compare nodes by.
type NodeId = Int

-- | A node represents a program in a network.
data Node = Node {
    -- | The program to run.
    _exe     :: Exe
    -- | Where to run the node.
  , _service :: Service
    -- | A unique identifier, can be set later by Disco.
  , _id      :: Maybe NodeId
    -- | Whether the node has access to its identifier.
  , _anon    :: Bool
  }

-- | A node constructor that avoids setting fields set later by Disco.
node :: Service -> Exe -> Node
node service exe = Node {
  _exe = exe, _service = service, _id = Nothing, _anon = False }

-- | Edges define the network topology and thus the communication channels.
data Edges =
    -- | Explicit edges between pairs of nodes.
    Edges [Edge]
    -- | Every pair of nodes can communicate directly.
  | CompleteGraph
    -- | Alternatively a shorthand may be specified.
  | EdgesShorthand EdgesShorthand

-- | Shorthand definitions for network topologies.
data EdgesShorthand =
    -- | Every node can communicate with ID+1 and ID-1.
  UndirectedRing

-- | An edge is a directed channel from one node to another.
data Edge = Edge { _from :: NodeId, _to :: NodeId }

-- | A service is capable of running nodes.
newtype Service = Service { _startNodes :: [Node] -> IO () }

-- | Description of an program that a node can run.
data Exe =
  -- | Download and install an executable from a git URL.
    Git { _url :: String, _exe :: String }
  -- | Download and install an executable from Hackage.
  | Hackage { _package :: String, _exe :: String }
  -- | Run a service-specific default program.
  | ServiceDefault
  -- | Run the given distributed algorithm.
  | ExeAlgorithm Algorithm
  deriving (Read, Show)

-- ** NOTE: The code below is currently NOT used.

-- | A message passing interface that adheres to the network topology.
data Messaging = Messaging {
    -- | Attempt to send a message to a node.
    _send :: Message -> NodeId -> IO ()
    -- | Register a handler for incoming messages.
  , _recv :: (Message -> NodeId -> IO ()) -> IO ()
  }

-- | You can read/show messages yourself.
type Message = String

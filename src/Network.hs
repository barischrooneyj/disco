{-# LANGUAGE DuplicateRecordFields #-}

module Network where

-- * A model of a network and its nodes.

import           Algorithm (Algorithm)

-- | A network consists simply of nodes and edges.
data Network = Network { _nodes :: [Node], _edges :: Edges }

-- | A network constructor where nodes are given new unique IDs.
network :: Edges -> [Node] -> Network
network edges nodes = Network { _nodes = newUniqueIds nodes, _edges = edges }

-- | By convention identifiers start at 1.
uniqueIds :: [NodeId]
uniqueIds = [1..]

-- | Nodes with new unique IDs.
newUniqueIds :: [Node] -> [Node]
newUniqueIds = zipWith applyNodeId uniqueIds
  where applyNodeId nId n = n { _id = Just nId }

-- | A simple type to compare nodes with.
type NodeId = Int

-- | A node represents a program in a network.
data Node = Node {
    -- | The program to run.
    _exe     :: Exe
    -- | How to run this node.
  , _service :: Service
    -- | A unique identifier set by Disco.
  , _id      :: Maybe NodeId
    -- | Network information set by Disco.
  , _netInfo :: Maybe Edges
  }

-- | A node constructor that avoids setting fields set later by Disco.
node :: Service -> Exe -> Node
node service exe = Node {
  _id = Nothing, _exe = exe, _service = service, _netInfo = Nothing }

-- | Edges define communication within the artificial network.
data Edges =
    -- | Explicit edges between pairs of nodes.
    Edges [Edge]
    -- | Every pair of nodes can communicate directly.
  | CompleteGraph
    -- | Every node can communicate with ID+1 and ID-1.
  | UndirectedRing

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
  | EAlgorithm Algorithm
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

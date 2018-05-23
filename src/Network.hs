{-# LANGUAGE DuplicateRecordFields #-}

module Network where

-- * A model of a network and its nodes.

-- | A network consists simply of nodes and edges.
data Network = Network { _nodes :: [Node], _edges :: Edges }

-- | A network constructor where nodes are given new unique IDs.
network :: Edges -> [Node] -> Network
network edges nodes = Network { _nodes = newUniqueIDs nodes, _edges = edges }

-- | Nodes with new unique IDs.
newUniqueIDs :: [Node] -> [Node]
newUniqueIDs = zipWith applyNodeId [1..]
  where applyNodeId nId n = n { _id = Just nId }

-- | A simple type to compare nodes with.
type NodeID = Int

-- | A node represents a program in a network.
data Node = Node {
    -- | The program to run.
    _exe     :: Exe
    -- | How to run this node.
  , _service :: Service
    -- | An optional identifier.
  , _id      :: Maybe NodeID
    -- | Network information to enable messaging.
  , _netInfo :: Maybe Edges
  }

-- | A node constructor that avoids setting fields set later by 'network'.
node :: Service -> Exe -> Node
node service exe = Node {
  _id = Nothing, _exe = exe, _service = service, _netInfo = Nothing }

-- | Edges define communication within the artificial network.
data Edges =
    -- | Explicit edges between pairs of nodes.
    Edges [Edge]
    -- | Every pair of nodes can communicate directly.
  | CompleteGraph
    -- | Every node can communicate with two neighbours in a ring.
  | UndirectedRing

-- | An edge is a directed channel from one node to another.
data Edge = Edge { _from :: NodeID, _to :: NodeID }

-- | A service is capable of running nodes.
newtype Service = Service { _startNodes :: [Node] -> IO () }

-- | Description of an executable that a node can run.
data Exe =
  -- | Download and install an executable from a git URL.
    Git { _url :: String, _exe :: String }
  -- | Download and install an executable from Hackage.
  | Hackage { _package :: String, _exe :: String }
  -- | The node will run its service's default executable.
  | ServiceDefault
  deriving (Read, Show)

-- | A message passing interface that adheres to the network topology.
data Messaging = Messaging {
    -- | Attempt to send a message to a node.
    _send :: Message -> NodeID -> IO ()
    -- | Register a handler for incoming messages.
  , _recv :: (Message -> NodeID -> IO ()) -> IO ()
  }

-- | You can read/show messages yourself.
type Message = String

{-# LANGUAGE DuplicateRecordFields #-}

module Network where

-- * A model of a network and its nodes.

-- ** An example hello world network.

-- | A few hello world nodes on the same network.
helloWorldNetwork :: Network
helloWorldNetwork = network CompleteGraph $ replicate 3 helloWorldNode

-- | A hello world node that runs in a local Docker container.
helloWorldNode :: Node
helloWorldNode = node LocalDocker ServiceDefault

-- ** The data types related to a network and nodes.

-- | A network consists simply of nodes and edges.
data Network = Network { _nodes :: [Node], _edges :: Edges }

-- | A 'Network' constructor where nodes are given new unique IDs.
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
  _exe     :: Exe,
  -- | Where to run the node.
  _service :: Service,
  -- | An optional identifier, should be unique.
  _id      :: Maybe NodeID,
  -- | Enables artificial network based messaging.
  _netInfo :: Maybe Edges
  }

-- | A 'Node' constructor that avoids setting fields set later by 'network'.
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

-- | Currently we only support locally hosted Docker containers.
data Service = LocalDocker | LocalProcess

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
  _send :: Message -> NodeID -> IO (),
  -- | Register a handler for incoming messages.
  _recv :: (Message -> NodeID -> IO ()) -> IO ()
  }

-- | You can read/show messages yourself.
type Message = String

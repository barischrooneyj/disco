{-# LANGUAGE DuplicateRecordFields #-}

module Network where

-- * A model of a network and its nodes.

-- ** Examples of data types.

-- | A hello world-style example node.
helloWorldOnDocker :: Node
helloWorldOnDocker = Node {
  -- ^ We will assign a unique one at start.
  _id = Nothing,
  -- ^ Start the node in a local Docker container.
  _service = LocalDocker,
  -- ^ The program to run.
  _exe = Default
  }

-- | A complete graph of 5 of the same hello world node.
exampleNetwork :: Network
exampleNetwork = Network {
  _nodes = replicate 1 helloWorldOnDocker,
  _edges = CompleteGraph
  }

-- ** The data types necessary for a network.

-- | A network is simply some of nodes and edges.
data Network = Network { _nodes :: [Node], _edges :: Edges }

-- | Either a set of edges or a shorthand definition.
data Edges = Edges [Edges] | CompleteGraph | Ring

-- | A simple type to compare nodes with.
type NodeID = Int

-- | A node represents a program in a network.
data Node = Node {
  -- ^ A unique identifier.
  _id      :: Maybe NodeID,
  -- ^ The program to run.
  _exe     :: Exe,
  -- ^ Where to start the node.
  _service :: Service
  -- ^ Information to enable high-level messaging.
  -- _messaging :: Messaging
  }

-- | Nodes are compared by identifier.
instance Eq Node where
  a == b = _id a == _id b
instance Ord Node where
  a `compare` b = _id a  `compare` _id b

-- | An edge is a directed channel from one node to another.
data Edge = Edge { _from :: NodeID, _to :: NodeID }

-- | Two edges are equal if they point the same way between the same nodes.
instance Eq Edge where
  a == b = _from a == _from b && _to a == _to b

-- | Currently we only support locally hosted Docker containers.
data Service = LocalDocker

-- | Information on an executable for a node to run.
data Exe =
  -- ^ Download and install an executable from a git URL.
    Git { _url :: String, _exe :: String }
  -- ^ Download and install an executable from Hackage.
  | Hackage { _package :: String, _exe :: String }
  -- ^ Inform the node to run the default executable.
  | Default
  deriving (Read, Show)

-- | A simple message passing interface available to nodes.
--
-- Controlling these functions allows us to artificially modify the topology and
-- performance of the network.
data Messaging = Messaging {
  -- ^ Attempt to send a message to a node.
  _send :: Message -> NodeID -> IO (),
  -- ^ Register a handler for incoming messages.
  _recv :: (Message -> NodeID -> IO ()) -> IO ()
  }

-- | You can read/show messages yourself.
type Message = String

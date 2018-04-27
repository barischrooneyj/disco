{-# LANGUAGE DuplicateRecordFields #-}

module Network where

-- * A model of a network and its nodes.

import           Data.Set (Set)
import qualified Data.Set as Set

-- ** An example network.

-- | A hello world-style example node.
helloWorldOnDocker = Node {
  -- ^ Note that setting an ID is optional, we can assign one automatically.
  _id = Nothing,
  -- ^ Launch the node as a locally hosted Docker container.
  _launch = Docker,
  -- ^ Directions to the executable to run.
  -- _exe = Git { _url = "https://github.com/barischrooneyj/hijohn", _exe = "hijohn-exe" }
  _exe = Default
  -- _messaging =
  }

-- | A triangle one-way network of hello world nodes.
exampleNetwork = Network {
  _nodes = [
      helloWorldOnDocker, helloWorldOnDocker, helloWorldOnDocker ],
  _edges = CompleteGraph
  }

-- ** The data types.

-- | A network is simply some of nodes and edges.
data Network = Network { _nodes :: [Node], _edges :: Edges }

-- | Either a set of edges or a shorthand definition.
data Edges = Edges [Edges] | CompleteGraph

-- | A simple type to compare nodes with.
type NodeID = Int

-- | A node is an independent system in a network.
data Node = Node {
  -- ^ Unique identifier.
  _id     :: Maybe NodeID,
  -- ^ Which executable to run.
  _exe    :: Exe,
  -- ^ Method of launching the node.
  _launch :: Launch
  -- ^ A simple message passing interface.
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
data Launch = Docker

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

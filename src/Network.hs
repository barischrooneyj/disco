{-# LANGUAGE DuplicateRecordFields #-}

module Network where

-- * A model of a network and its nodes.

import           Data.ByteString (ByteString)
import           Data.Set        (Set)
import qualified Data.Set        as Set

-- ** An example network.

-- | A hello world-style 'Node'. Will run on a Docker container on your machine.
helloWorldOnDocker = Node {
  _launch = Docker,
  _exe = Git { _url = "https://github.com/barischrooneyj/hijohn", _exe = "hijohn-exe" },
  _messaging = 
  }

-- | A triangle shaped network of hello world nodes.
example = Network {
  _nodes = Set.fromList [
      helloWorldOnDocker, helloWorldOnDocker, helloWorldOnDocker ]
                  }

-- ** The data types.

-- | A network is simply a set of nodes and edges.
data Network = Network { _nodes :: Set Node, _edges :: Set Edge }

-- | A simple type to compare nodes with.
type NodeID = Int

-- | A node is an independent system in a network.
data Node = Node {
  -- ^ Unique identifier.
  _id        :: NodeID,
  -- ^ How to launch the node.
  _launch    :: Launch,
  -- ^ The executable to run.
  _exe       :: Exe,
  -- ^ A simple message passing interface.
  _messaging :: Messaging
  }

-- | Nodes are equal if they have the same identifier.
instance Eq Node where
  a == b = _id a == _id b

-- | An edge is a directed channel from one node to another.
data Edge = Edge { _from :: NodeID, _to :: NodeID }

-- | Two edges are equal if they point the same way between the same nodes.
instance Eq Edge where
  a == b = _from a == _from b && _to a == _to b

-- | Currently we only support Docker containers on host-machines.
data Launch = Docker

-- | The executable to run on a node.
data Exe =
    Git { _url :: String, _exe :: String }
  | Hackage { _package :: String, _exe :: String }

-- | A simple message passing interface. Controlling these functions allows us
-- to artificially modify the topology and performance of the network.
data Messaging = Messaging {
  -- ^ Attempt to send a message to a node.
  _send :: Message -> NodeID -> IO (),
  -- ^ Register a handler for incoming messages.
  _recv :: (Message -> NodeID -> IO ()) -> IO ()
  }

-- | You can parse/serialize messages yourself.
type Message = ByteString

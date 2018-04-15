module Network where

import Data.Set (Set)

type Message = String
type ID = Int

-- | A protocol abstraction for message passing.
data Messaging = Messaging {
  -- ^ Attempt to send a message to the node with given ID.
  send :: Message -> ID -> IO (),
  -- ^ Listen for incoming messages and run a handler on each.
  recv :: (Message -> ID -> IO ()) -> IO ()
  }

-- | A node consists of an identifier and some IO function to run (which
-- receives a message passing abstraction as an argument).
data Node = Node { nid :: ID, f :: Messaging -> IO () }

-- | Nodes are equal if they have the same identifier.
instance Eq Node where
  a == b = nid a == nid b

-- | An edge is a directed channel between two nodes.
data Edge = Edge { from :: ID, to :: ID }

-- | Edges are equal if they point the same direction between the same nodes.
instance Eq Edge where
  a == b = from a == from b && to a == to b

-- | A network is a set of nodes and a set of edges.
data Network = Network { nodes :: Set Node, edges :: Set Edge }

-- | Launch a complete (fully connected) network topology of nodes.
-- launchCompleteTopology :: Set Node -> Hardware -> IO ()
-- launchCompleteTopology = 

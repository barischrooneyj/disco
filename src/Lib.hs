{-# LANGUAGE DuplicateRecordFields #-}

module Lib where

import           Data.Set           (Set)
import           System.Environment (getEnvironment)

-- | Data types for describing a network of message passing nodes.

type ID = Int

-- | A message of type 'm', a recipient ID and sender ID.
data Message m = Message { body :: m, from :: ID, to :: ID }

-- | An event that a node emits.
data Event s m =
  -- ^ Updating a node's state.
    Internal (s -> s)
  -- ^ Sending a message to a node.
  | Send (m, ID)
  -- ^ Receipt of a message from a node.
  | Receipt (m, ID)

-- | An independent system in the network.
data Node s m = Node {

  -- | These fields define the node's logic.

  -- ^ The node's initial state.
  initState :: s,
  -- ^ Identifier (that should be unique).
  id        :: ID,
  -- ^ Receive a message and maybe perform events.
  recv      :: Message m -> [Event s m],

  -- | These fields define the hardware interface.

  -- ^ Send a message to a node.
  send      :: Message m -> IO (),
  -- ^ Listen for incoming messages and emit with given function.
  listen    :: (m -> IO ()) -> IO ()
  }

-- | A directed channel between two nodes.
data Edge = Edge {
  -- | ID of sending node.
  from  :: ID,
  -- | ID of sending node.
  to    :: ID,
  -- | Maybe an artificially added delay in seconds.
  delay :: Maybe Int
  }

-- | A number of nodes and edges between them.
data Network s m = Network {
  -- ^ The independent systems comprising the network.
  nodes :: [Node s m],
  -- ^ The communication channels between nodes.
  edges :: [Edge]
  }

-- | A 'Network' constructor that assumes a complete network topology, complete
-- meaning there is an undirected channel between each pair of nodes.
someFunc :: IO ()
someFunc = getEnvironment >>= print

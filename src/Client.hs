module Client where

-- * Interface available to services running nodes.
--
-- Higher level functionality, such as the evaluation of DSLs for distributed
-- algorithms, will be contained in separate modules.

import           Network (NodeId)

-- | A message passing interface that adheres to the network topology.
data Messaging = Messaging {
    -- | Attempt to send a message to a node.
    _send :: Message -> NodeId -> IO ()
    -- | Register a handler for incoming messages.
  , _recv :: (Message -> NodeId -> IO ()) -> IO ()
  }

-- | You can read/show messages yourself.
type Message = String

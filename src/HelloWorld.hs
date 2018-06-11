module HelloWorld where

import           Algorithm      (Algorithm (..))
import           Data.Maybe     (fromJust)
import           Network        (Edges (..), EdgesShorthand (..), Network, Node,
                                 Program (..), network, node)
import           Service.Docker (localDocker)

-- ** An example hello world network.

-- | A few hello world nodes on the same network.
helloWorldNetwork :: Network
helloWorldNetwork = fromJust $ network (EdgesShorthand CompleteGraph) $ replicate 1 helloWorldNode

-- | A hello world node that runs in a local Docker container.
helloWorldNode :: Node
helloWorldNode = node localDocker $ Algorithm Foo

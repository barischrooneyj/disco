module HelloWorld where

import           Data.Graph.Generators.Classic (moebiusKantorGraph)
import           Data.Maybe                    (fromJust)

import           Algorithm                     (Algorithm (..))
import           Network                       (Edges (..), Network, Node,
                                                Program (..), network, node)
import           Service.Docker                (localDocker)


-- ** An example hello world network.

-- | A few hello world nodes on the same network.
helloWorldNetwork :: Network
helloWorldNetwork = fromJust $
  network (Graph moebiusKantorGraph) $ replicate 16 helloWorldNode

-- | A hello world node that runs in a local Docker container.
helloWorldNode :: Node
helloWorldNode = node localDocker $ Algorithm Foo

module HelloWorld where

import           Algorithm      (Algorithm (..))
import           Network        (Edges (CompleteGraph), Exe (EAlgorithm),
                                 Network, Node, network, node)
import           Service.Docker (localDocker)

-- ** An example hello world network.

-- | A few hello world nodes on the same network.
helloWorldNetwork :: Network
helloWorldNetwork = network CompleteGraph $ replicate 1 helloWorldNode

-- | A hello world node that runs in a local Docker container.
helloWorldNode :: Node
helloWorldNode = node localDocker $ EAlgorithm Foo

module HelloWorld where

import           Network
import           Service.Docker (localDocker)

-- ** An example hello world network.

-- | A few hello world nodes on the same network.
helloWorldNetwork :: Network
helloWorldNetwork = network CompleteGraph $ replicate 3 helloWorldNode

-- | A hello world node that runs in a local Docker container.
helloWorldNode :: Node
helloWorldNode = node localDocker ServiceDefault

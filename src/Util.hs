module Util where

import           Network (Network (..), Node (..))

-- | The network but all nodes have unique IDs, maybe.
--
-- The IDs of nodes that have them will not be altered, if there is a duplicate
-- among them we return Nothing.
applyUniqueIDs :: Network -> Network
applyUniqueIDs network =
  let newNodes = map (\(i, n) -> n { _id = Just i }) nodesZip
      nodesZip = zip [1..] $ _nodes network
  in network { _nodes = newNodes}


module Start where

-- * Start networks and nodes on services.

import           Data.ByteString.Lazy.Char8 (unpack)
import           Data.Maybe                 (fromJust)
import qualified System.Directory           as Dir
import           System.Exit                (ExitCode (..))
import           System.FilePath            ((</>))
import qualified System.Process.Typed       as Proc

import           Network                    (Edge (..), Edges (..), EdgesShorthand (..),
                                             Network (..), Node (..), NodeId,
                                             Service (..))

-- | Start the given network!
startNetwork :: Network -> IO ()
startNetwork network = do
  print network
  print $ toStandardEdges network
  case toStandardEdges network of
    Nothing -> print "NOTE: Not a standard network"
    Just standardNetwork -> case _edges standardNetwork of
      Edges _setEdges -> do
        print "NOTE: We are not yet passing artificial network information to nodes"
        startNodes $ _nodes standardNetwork
      _               -> print "NOTE: This is not a standard network"

-- | A network in standard format does NOT use a shorthand for edges.
toStandardEdges :: Network -> Maybe Network
toStandardEdges network =
  case _edges network of
    EdgesShorthand UndirectedRing ->
      let maxId = length $ _nodes network
          edges = foldl
            (\es n -> undirectedEdges (fromJust $ _identifier n) maxId ++ es)
            []
            (_nodes network)
      in pure network { _edges = Edges edges }
    EdgesShorthand _ -> Nothing
    -- | Otherwise already in standard format.
    _ -> pure network

-- | The edges for a node in an undirected ring.
undirectedEdges :: NodeId -> NodeId -> [Edge]
undirectedEdges nId maxId
  | nId == 1     = [Edge nId maxId    , Edge nId $ nId + 1]
  | nId == maxId = [Edge nId $ nId - 1, Edge nId 1        ]
  | otherwise    = [Edge nId $ nId - 1, Edge nId $ nId + 1]

-- | Start a complete graph.
--
-- Currently this is done by starting all nodes on the service of the first
-- node. This works because we know we have one service currently. When we have
-- multiple services implemented we will ask each service to start the
-- respective set of nodes, at this point our messaging functionality must be
-- able to handle service boundaries.
startNodes :: [Node] -> IO ()
startNodes []          = print "No nodes to start complete graph"
startNodes nodes@(n:_) = _startNodes (_service n) nodes


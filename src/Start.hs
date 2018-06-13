module Start where

-- * Start networks and nodes on services.

import           Data.Graph.Generators (GraphInfo (..))
import           Text.Pretty.Simple    (pPrint)

import           Network               (Edge (..), Edges (..), Network (..),
                                        Node (..), Service (..))

-- | Start the given network!
startNetwork :: Network -> IO ()
startNetwork network = do
  pPrint $ toStandardEdges network
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
    Graph graphInfo ->
      pure $ network { _edges = Edges generatorEdges }
      where generatorEdges = map (uncurry Edge) (edges graphInfo)
    -- | Otherwise already in standard format.
    _                        -> pure network

-- | Start running some nodes.
--
-- Currently this is done by starting all nodes on the service of the first
-- node. This works because we know we have one service currently. When we have
-- multiple services implemented we will ask each service to start the
-- respective set of nodes, at this point our messaging functionality must be
-- able to handle service boundaries.
startNodes :: [Node] -> IO ()
startNodes []          = print "No nodes to start complete graph"
startNodes nodes@(n:_) = _startNodes (_service n) nodes


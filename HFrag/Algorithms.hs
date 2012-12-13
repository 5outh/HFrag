module HFrag.Algorithms(
	tsort
)where

import HFrag
import HFrag.Instances
import HFrag.Types
-- for pathfinding
import Data.Ord(compare)
import Data.Function(on)
   
{- topological sort -}
tsort g@(Graph vs es) = tsort' [] (noInbound g) g
  where noInbound g@(Graph vs es) = filter (null . flip inEdges g) (map info vs)
        tsort' l [] (Graph _ []) = reverse l
        tsort' l [] _            = error "At least one cycle in this graph"
        tsort' l (n:s) g         = tsort' (n:l) s' g'
          where outNodes = map to $ outEdges n g
                outbound = outEdges n g
                g' = foldr removeEdge g outbound
                s' = s ++ filter (null . flip inEdges g') (map info outNodes)
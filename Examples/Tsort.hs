import HFrag
import HFrag.Types
import HFrag.Instances

people = Graph
  (map Node ["Ron","April","Ann","Leslie","Andy","Jerry"])
  [Edge (Node "Ron") (Node "April"), Edge (Node "Ron") (Node "Andy"), Edge (Node "Ron") (Node "Ann"),
   Edge (Node "April") (Node "Ann"), Edge (Node "April") (Node "Andy"), Edge (Node "Leslie") (Node "April"),
   Edge (Node "Leslie") (Node "Ron"), Edge (Node "Leslie") (Node "Andy"), Edge (Node "Ann") (Node "Andy"),
   Edge (Node "Ann") (Node "Jerry"), Edge (Node "Andy") (Node "Jerry")]
   
tsort g@(Graph vs es) = tsort' [] (noInbound g) g
  where noInbound g@(Graph vs es) = filter (null . flip inEdges g) (map info vs)
        tsort' l [] (Graph _ []) = reverse l
        tsort' l [] _            = error "At least one cycle in this graph"
        tsort' l (n:s) g         = tsort' (n:l) s' g'
          where outNodes = map to $ outEdges n g
                outbound = outEdges n g
                g' = foldr removeEdge g outbound
                s' = s ++ filter (null . flip inEdges g') (map info outNodes)
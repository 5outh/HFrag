module HFrag(
  outEdges,
  inEdges,
  findVertex,
  findEdge,
  modifyVertex,
  modifyEdgeWeight
)where

-- for pathfinding
import Data.Ord(compare)
import Data.Function(on)
import HFrag.Types
import HFrag.Instances

sample = Graph 
  [WVNode A False (Number 0), WVNode B False Inf,
   WVNode C False Inf]
  [WEdge (WVNode A False (Number 0)) (WVNode B False Inf) 2,
   WEdge (WVNode A False (Number 0)) (WVNode C False Inf) 4,
   WEdge (WVNode B False Inf) (WVNode C False Inf) 3]
   
conns :: (Eq a) => (Edge a -> a) -> a -> Graph a -> [Edge a]
conns f x (Graph _ es) = filter ((==x) . f) es

outEdges x = conns from x

inEdges x = conns to x

findVertex :: (Eq a) => a -> Graph a -> a
findVertex a (Graph vs es) = head $ filter (==a) vs

findEdge :: (Eq a) => a -> a -> Graph a -> Edge a
findEdge a b (Graph _ es) = head $ filter (\x -> from x == a && to x == b) es

findEdgesContaining :: (Eq a) => a -> Graph a -> [Edge a]
findEdgesContaining v g@(Graph _ es) = filter (edgeContains v) es
  where edgeContains v e' = from e' == v || to e' == v  

-- ex : modifyVertex (fmap succ) (head $ vertices sample) sample
modifyVertex :: (Eq a) => (a -> a) -> a -> Graph a -> Graph a
modifyVertex f v g@(Graph vs es) = Graph vs' es'
  where vs' = f v : filter (/= v) vs
        cEdges = findEdgesContaining v g
        es' = map (fmap f) cEdges ++  filter (`notElem` cEdges) es
		
modifyEdgeWeight :: (Eq a) => (Unbounded Int -> Unbounded Int) -> Edge a -> Graph a -> Graph a
modifyEdgeWeight f e@(WEdge a b w) (Graph vs es) = Graph vs (modifyWeight f e : filter (/= e) es)
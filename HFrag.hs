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

sampleEdge = WEdge (WVNode A False (Number 0)) (WVNode B False Inf) 2
sampleVertex = WVNode A False (Number 0)

outEdges :: (Vertex v, Eq a) => a -> Graph (v a) -> [Edge (v a)]
outEdges x (Graph _ es) = filter ((==x) . info . from) es

inEdges :: (Vertex v, Eq a) => a -> Graph (v a) -> [Edge (v a)]
inEdges x (Graph _ es) = filter ((==x) . info . to) es

adjacent :: (Eq a) => a -> a -> Graph a -> Bool
adjacent a b g = not $ null $ filter (\x -> from x == a && to x == b) $ edges g

findVertex :: (Vertex v, Eq a) => a -> Graph (v a) -> v a
findVertex a (Graph vs es) = head $ filter ((==a) . info) vs

findEdge :: (Vertex v, Eq a) => a -> a -> Graph (v a) -> Edge (v a)
findEdge a b (Graph _ es) = head $ filter (\x -> (info $ from x) == a && (info $ to x) == b) es

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
module HFrag(
  outEdges,
  inEdges,
  findVertex,
  findEdge,
  modifyVertex,
  modifyEdgeWeight,
  findEdgesContaining,
  edgeContains,
  removeEdge,
  removeVertex,
  sample,
  sample2
)where

import HFrag.Types
import HFrag.Instances

{-@todo: Want to move to this! Less duplication!
sample = Graph
  [WVNode A False (Number 0), WVNode B False Inf,
   WVNode C False Inf]
  [WEdge A B 2,
   WEdge A C 4,
   WEdge B C 3]
-}
sample = Graph
  [WVNode A False (Number 0), WVNode B False Inf,
   WVNode C False Inf]
  [WEdge (WVNode A False (Number 0)) (WVNode B False Inf) 2,
   WEdge (WVNode A False (Number 0)) (WVNode C False Inf) 4,
   WEdge (WVNode B False Inf) (WVNode C False Inf) 3]

--unfortunately this doesn't make items point to the same thing :(
sample2 = Graph 
 [WVNode A False (Number 0), WVNode B False Inf]
 [WEdge (head $ vertices sample2) (last $ vertices sample2) 2]
   
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

removeEdge :: (Eq a) => Edge a -> Graph a -> Graph a
removeEdge x g@(Graph vs es) = Graph vs es'
  where es' = filter (/=x) es
  
findEdgesContaining :: (Eq a) => a -> Graph a -> [Edge a]
findEdgesContaining v g@(Graph _ es) = filter (edgeContains v) es

edgeContains :: (Eq a) => a -> Edge a -> Bool
edgeContains v e' = from e' == v || to e' == v  

removeVertex :: (Eq a) => a -> Graph a -> Graph a
removeVertex v g@(Graph vs es) = Graph vs' es'
  where vs' = filter (/=v) vs
        es' = filter (`notElem` (findEdgesContaining v g)) es 

-- ex : modifyVertex (fmap succ) (head $ vertices sample) sample
-- this is broken! Modifies both of cEdges.
modifyVertex :: (Eq a) => (a -> a) -> a -> Graph a -> Graph a
modifyVertex f v g@(Graph vs es) = Graph vs' es
  where vs' = f v : filter (/= v) vs
        cEdges = findEdgesContaining v g
        es' = map (fmap f) cEdges ++  filter (`notElem` cEdges) es
		
modifyEdgeWeight :: (Eq a) => (Unbounded Int -> Unbounded Int) -> Edge a -> Graph a -> Graph a
modifyEdgeWeight f e@(WEdge a b w) (Graph vs es) = Graph vs (modifyWeight f e : filter (/= e) es)
  

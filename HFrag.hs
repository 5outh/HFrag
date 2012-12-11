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
  [WVVertex A False (Just 0), WVVertex B False Nothing,
   WVVertex C False Nothing, WVVertex D False Nothing,
   WVVertex E False Nothing]
  [WEdge 2 (WVVertex A False (Just 0)) (WVVertex B False Nothing), 
   WEdge 4 (WVVertex A False (Just 0)) (WVVertex C False Nothing), 
   WEdge 6 (WVVertex B False Nothing) (WVVertex C False Nothing),
   WEdge 8 (WVVertex B False Nothing) (WVVertex E False Nothing),
   WEdge 9 (WVVertex E False Nothing) (WVVertex C False Nothing), 
   WEdge 4 (WVVertex C False Nothing) (WVVertex D False Nothing),
   WEdge 1 (WVVertex D False Nothing) (WVVertex E False Nothing)]
   
conns :: (Eq a) => (Edge a -> Vertex a) -> Vertex a -> Graph a -> [Edge a]
conns f x (Graph _ es) = filter ((==x) . f) es

outEdges x = conns from x

inEdges x = conns to x

findVertex :: (Eq a) => a -> Graph a -> Vertex a
findVertex a (Graph vs es) = head $ filter ((==a) . name) vs

findEdge :: (Eq a) => Vertex a -> Vertex a -> Graph a -> Edge a
findEdge a b (Graph _ es) = head $ filter (\x -> from x == a && to x == b) es

findEdgesContaining :: (Eq a) => Vertex a -> Graph a -> [Edge a]
findEdgesContaining v g@(Graph _ es) = filter (edgeContains v) es
  where edgeContains v e' = from e' == v || to e' == v  

mapVertices :: (Vertex a -> Vertex a) -> Edge a -> Edge a
mapVertices f (Edge a b) = Edge (f a) (f b)
mapVertices f (WEdge w a b) = WEdge w (f a) (f b)
  
-- ex : modifyVertex (fmap succ) (head $ vertices sample) sample
modifyVertex :: (Eq a) => (Vertex a -> Vertex a) -> Vertex a -> Graph a -> Graph a
modifyVertex f v g@(Graph vs es) = Graph vs' es'
  where vs' = f v : filter (/= v) vs
        cEdges = findEdgesContaining v g
        es' = map (mapVertices f) cEdges ++  filter (`notElem` cEdges) es

modifyEdgeWeight :: (Eq a) => (Int -> Int) -> Edge a -> Graph a -> Graph a
modifyEdgeWeight f e@(WEdge w a b) (Graph vs es) = Graph vs (WEdge (f w) a b : filter (/= e) es)
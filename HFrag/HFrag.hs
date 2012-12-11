module HFrag(
  Edge(Edge,WEdge),
  Vertex(Vertex,VVertex,WVertex,WVVertex),
  Graph(Graph),
  Letter,
  (>+>),
  outEdges,
  inEdges,
  plus,
  findVertex,
  findEdge,
  modifyVertex,
  modifyEdgeWeight
)where

import Data.Ord(compare)
import Data.Function(on)

data Edge a = Edge  { from :: Vertex a, to :: Vertex a } --how to get rid of error from calling weight?
            | WEdge { eWeight :: Int, from :: Vertex a, to:: Vertex a } deriving (Show, Eq)

data Vertex a = Vertex   { name :: a }| 
              VVertex  { name :: a, visited :: Bool } | --Visitable
              WVertex  { name :: a, nWeight :: Maybe Int } |  --Weighted
              WVVertex { name :: a, visited :: Bool, nWeight :: Maybe Int} deriving (Show, Eq) --Weighted/Visitable
			  
data Graph a = Graph{ vertices :: [Vertex a], edges :: [Edge a] } deriving (Show, Eq)

data Letter = A|B|C|D|E|F|G|H|I|J|K|L|M|N|O|P deriving (Show, Eq, Enum)

infixr >+>

Just a  >+> Just b  = Just (a+b)
Just a  >+> Nothing = Just a
Nothing >+> Just a  = Just a
Nothing >+> Nothing = Nothing

instance Functor Vertex where
  fmap f (Vertex a) = Vertex (f a)
  fmap f (VVertex a v) = VVertex (f a) v
  fmap f (WVertex a w) = WVertex (f a) w
  fmap f (WVVertex a v w) = WVVertex (f a) v w

instance Functor Edge where
  fmap f (Edge a b) = Edge (fmap f a) (fmap f b)
  fmap f (WEdge w a b) = WEdge w (fmap f a) (fmap f b)
  
instance Functor Graph where
  fmap f (Graph vs es) = Graph (map (fmap f) vs) (map (fmap f) es)
  
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
   
conns :: (Eq a) => (Edge a -> Vertex a) -> (Vertex a) -> Graph a -> [Edge a]
conns f x (Graph _ es) = filter ((==x) . f) es

outEdges x = conns from x

inEdges x = conns to x

plus x (WVertex a w) = WVertex a (w >+> Just x)
plus x (WVVertex a b w) = WVVertex a b (w >+> Just x)

findVertex :: (Eq a) => a -> Graph a -> Vertex a
findVertex a (Graph vs es) = head $ filter ((==a) . name) vs

findEdge :: (Eq a) => Vertex a -> Vertex a -> Graph a -> Edge a
findEdge a b (Graph _ es) = head $ filter (\x -> from x == a && to x == b) es

findEdgesContaining :: (Eq a) => Vertex a -> Graph a -> [Edge a]
findEdgesContaining v g@(Graph _ es) = filter (edgeContains v) es
  where edgeContains v e' = from e' == v || to e' == v  

--Will not modify edge pointers yet :(
modifyVertex :: (Eq a) => (Vertex a -> Vertex a) -> Vertex a -> Graph a -> Graph a
modifyVertex f v g@(Graph vs es) = Graph vs' es
  where vs' = f v : filter (/= v) vs
        edges = findEdgesContaining v g
		
modifyEdgeWeight :: (Eq a) => (Int -> Int) -> Edge a -> Graph a -> Graph a
modifyEdgeWeight f e@(WEdge w a b) (Graph vs es) = Graph vs (WEdge (f w) a b : filter (/= e) es)
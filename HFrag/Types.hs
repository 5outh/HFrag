module HFrag.Types(
  Edge(Edge, WEdge, from, to, eWeight),
  Graph(Graph, vertices, edges),
  Letter(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P),
  Unbounded(Number, Inf, NInf),
  VNode(VNode),
  WNode(WNode),
  WVNode(WVNode),
  Node(Node),
  Visitable(visit, unvisit, isVisited),
  Weighted(modifyWeight, getWeight),
)where

data Graph a = Graph { vertices :: [a], edges :: [Edge a] } deriving (Show, Eq)

data Edge a = Edge  { from :: a, to :: a } --how to get rid of error from calling weight?
            | WEdge { from :: a, to :: a, eWeight :: Unbounded Int } deriving (Show, Eq)


data Node a = Node a deriving (Show, Eq) --normal
data VNode a = VNode a Bool deriving (Show, Eq) --visitable
--probably want these to be Unbounded (Num b), but not sure how yet
data WNode a = WNode a (Unbounded Int) deriving (Show, Eq) --weighted		  
data WVNode a = WVNode a Bool (Unbounded Int) deriving (Show, Eq) --weighted, visitable

data Letter = A|B|C|D|E|F|G|H|I|J|K|L|M|N|O|P deriving (Show, Eq, Enum)

data Unbounded a = Number a | Inf | NInf deriving (Show, Eq)
			  
class Visitable a where
  visit :: a -> a
  unvisit :: a -> a
  isVisited :: a -> Bool

class Weighted a where
  modifyWeight :: (Unbounded Int -> Unbounded Int) -> a -> a
  getWeight    :: a -> Unbounded Int
module HFrag(
  Edge(Edge,WEdge),
  Node(Node,VNode,WNode,WVNode),
  Graph(Graph),
  Letter,
  (>+>),
  outEdges,
  inEdges,
  plus,
  findNode,
  findEdge,
  modifyNode,
  modifyEdge
)where

import Data.Ord(compare)
import Data.Function(on)

data Edge a = Edge  { from :: a, to :: a } --how to get rid of error from calling weight?
            | WEdge { eWeight :: Int, from :: a, to:: a } deriving (Show, Eq)

data Node a = Node   { name :: a }| 
              VNode  { name :: a, visited :: Bool } | --Visitable
              WNode  { name :: a, nWeight :: Maybe Int } |  --Weighted
              WVNode { name :: a, visited :: Bool, nWeight :: Maybe Int} deriving (Show, Eq) --Weighted/Visitable
			  
data Graph a = Graph{ vertices :: [Node a], edges :: [Edge a] } deriving (Show, Eq)

data Letter = A|B|C|D|E|F|G|H|I|J|K|L|M|N|O|P deriving (Show, Eq, Enum)

infixr >+>

Just a  >+> Just b  = Just (a+b)
Just a  >+> Nothing = Just a
Nothing >+> Just a  = Just a
Nothing >+> Nothing = Nothing

instance Functor Node where
  fmap f (Node a) = Node (f a)
  fmap f (VNode a v) = VNode (f a) v
  fmap f (WNode a w) = WNode (f a) w
  fmap f (WVNode a v w) = WVNode (f a) v w

instance Functor Edge where
  fmap f (Edge a b) = Edge (f a) (f b)
  fmap f (WEdge w a b) = WEdge w (f a) (f b)
  
instance Functor Graph where
  fmap f (Graph v e) = Graph (fmap (fmap f) v) (fmap (fmap f) e)
  
sample = Graph 
  [WVNode A False (Just 0), WVNode B False Nothing,
   WVNode C False Nothing, WVNode D False Nothing,
   WVNode E False Nothing]
  [WEdge 2 A B, WEdge 4 A C, WEdge 6 B C,
   WEdge 8 B E, WEdge 9 E C, WEdge 4 C D,
   WEdge 1 D E]
   
conns :: (Eq a) => (Edge a -> a) -> a -> Graph a -> [Edge a]
conns f x (Graph _ e) = filter ((==x) . f) e

outEdges x = conns from x

inEdges x = conns to x

plus x (WNode a w) = WNode a (w >+> Just x)
plus x (WVNode a b w) = WVNode a b (w >+> Just x)

findNode :: (Eq a) => a -> Graph a -> Node a
findNode a (Graph v e) = head $ filter ((==a) . name) v

findEdge :: (Eq a) => a -> a -> Graph a -> Edge a
findEdge a b (Graph v e) = head $ filter (\x -> from x == a && to x == b) e

--Will not modify edge pointers :(
modifyNode :: (Eq a) => (Node a -> Node a) -> Node a -> Graph a -> Graph a
modifyNode f node g@(Graph v e) = Graph v' e
  where v' = f node : filter (/= node) v

--This also won't modify edge pointers :(
modifyEdge :: (Eq a) => (Edge a -> Edge a) -> Edge a -> Graph a -> Graph a
modifyEdge f edge g@(Graph v e) = Graph v e'
  where e' = f edge : filter (/= edge) e
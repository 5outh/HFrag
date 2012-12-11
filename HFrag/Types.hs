module HFrag.Types(
  Edge(Edge, WEdge, from, to, eWeight),
  Vertex(Vertex, VVertex, WVertex, WVVertex, name, visited, nWeight),
  Graph(Graph),
  Letter(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P)
)where

data Edge a = Edge  { from :: Vertex a, to :: Vertex a } --how to get rid of error from calling weight?
            | WEdge { eWeight :: Int, from :: Vertex a, to:: Vertex a } deriving (Show, Eq)

data Vertex a = Vertex   { name :: a }| 
              VVertex  { name :: a, visited :: Bool } | --Visitable
              WVertex  { name :: a, nWeight :: Maybe Int } |  --Weighted
              WVVertex { name :: a, visited :: Bool, nWeight :: Maybe Int} deriving (Show, Eq) --Weighted/Visitable
			  
data Graph a = Graph{ vertices :: [Vertex a], edges :: [Edge a] } deriving (Show, Eq)

data Letter = A|B|C|D|E|F|G|H|I|J|K|L|M|N|O|P deriving (Show, Eq, Enum)
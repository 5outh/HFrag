module HFrag.Types(
  Edge(Edge, WEdge, from, to, eWeight),
  Vertex(Vertex, VVertex, WVertex, WVVertex, name, visited, nWeight),
  Graph(Graph),
  Letter(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P),
  Unbounded(Number, Inf, NInf)
)where

data Edge a = Edge  { from :: Vertex a, to :: Vertex a } --how to get rid of error from calling weight?
            | WEdge { eWeight :: Int, from :: Vertex a, to:: Vertex a } deriving (Show, Eq)

data Vertex a = Vertex   { name :: a }| 
              VVertex  { name :: a, visited :: Bool } | --Visitable
              WVertex  { name :: a, nWeight :: Maybe Int } |  --Weighted
              WVVertex { name :: a, visited :: Bool, nWeight :: Maybe Int} deriving (Show, Eq) --Weighted/Visitable
			  
data Graph a = Graph{ vertices :: [Vertex a], edges :: [Edge a] } deriving (Show, Eq)

data Letter = A|B|C|D|E|F|G|H|I|J|K|L|M|N|O|P deriving (Show, Eq, Enum)

data Unbounded a = Number a | Inf | NInf deriving (Show, Eq)

instance (Num a, Ord a) => Ord (Unbounded a) where
  compare (Number a) (Number b)
    | a == b = EQ
    | a <  b = LT
    | a >  b = GT
  compare (Number _) Inf = LT
  compare Inf (Number _) = GT
  compare (Number _) NInf = GT
  compare NInf (Number _) = LT
  compare Inf NInf = GT
  compare NInf Inf = LT

instance (Num a, Ord a) => Num (Unbounded a) where
  (Number a) + (Number b) = Number (a + b)
  
  (Number _) + Inf = Inf
  (Number _) + NInf = NInf
  
  Inf + Inf = Inf
  NInf + NInf = NInf
  NInf + Inf = undefined
  
  Inf + _ = Inf
  NInf + _ = NInf
  
  Number 0 * NInf = undefined
  Number 0 * Inf = undefined
  Inf * Number 0 = undefined
  NInf * Number 0 = undefined
  
  Inf * Inf = Inf
  NInf * NInf = NInf
  
  Inf * NInf = undefined
  NInf * Inf = undefined
  
  (Number _) * Inf = Inf
  (Number _) * NInf = NInf
  Inf * (Number _) = Inf
  NInf * (Number _) = NInf
  
  (Number a) * (Number b) = Number (a*b)
  
  abs n@(Number a) = if a < 0 then (Number $ negate a) else n
  abs Inf = Inf
  abs NInf = Inf
  
  signum Inf = Number 1
  signum NInf = Number (-1)
  signum (Number a) = if a < 0 then Number (-1) else Number 1
  
  fromInteger x = Number (fromIntegral x)
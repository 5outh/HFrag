module HFrag.Instances where

import HFrag.Types

instance Functor Node where
  fmap f (Node a) = Node (f a)
  
instance Functor VNode where
  fmap f (VNode a b) = VNode (f a) b

instance Functor WNode where
  fmap f (WNode a b) = WNode (f a) b
  
instance Functor WVNode where
  fmap f (WVNode a b c) = WVNode (f a) b c
  
instance Functor Edge where
  fmap f (Edge a b) = Edge (f a) (f b)
  fmap f (WEdge a b w) = WEdge (f a) (f b) w
  
instance Functor Graph where
  fmap f (Graph vs es) = Graph (map f vs) (map (fmap f) es)
  
instance Visitable (VNode a) where
  visit (VNode a _) = VNode a True
  unvisit (VNode a _) = VNode a False
  isVisited (VNode _ b) = b

instance Visitable (WVNode a) where
  visit     (WVNode a _ c) = WVNode a True c
  unvisit   (WVNode a _ c) = WVNode a False c
  isVisited (WVNode _ b _) = b
  
instance Weighted (WNode a) where
  modifyWeight f (WNode a b) = WNode a (f b)
  getWeight      (WNode _ b) = b

instance Weighted (WVNode a) where
  modifyWeight f (WVNode a b c) = WVNode a b (f c)
  getWeight      (WVNode _ _ c) = c

instance Weighted (Edge a) where
  modifyWeight f (Edge _ _) = error "Attempt to modify unweighted edge"
  modifyWeight f (WEdge a b c) = WEdge a b (f c)
  getWeight      (Edge _ _) = error "Attempt to get unweighted edge"
  getWeight      (WEdge _ _ c) = c

instance Vertex Node where
  info (Node a) = a
  
instance Vertex VNode where
  info (VNode a _) = a
  
instance Vertex WNode where
  info (WNode a _) = a
  
instance Vertex WVNode where
  info (WVNode a _ _) = a
  
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
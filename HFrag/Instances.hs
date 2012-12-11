module HFrag.Instances(
)where

import HFrag.Types

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
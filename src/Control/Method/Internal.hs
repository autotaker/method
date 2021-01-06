{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Control.Method.Internal
  ( TupleLike (..),
    Nil (Nil),
    (:*) ((:*)),
  )
where

class TupleLike a where
  type AsTuple a
  fromTuple :: AsTuple a -> a
  toTuple :: a -> AsTuple a

instance TupleLike Nil where
  type AsTuple Nil = ()
  fromTuple _ = Nil
  toTuple _ = ()

instance TupleLike (a :* Nil) where
  type AsTuple (a :* Nil) = a
  fromTuple a = a :* Nil
  toTuple (a :* Nil) = a

instance TupleLike (a :* b :* Nil) where
  type AsTuple (a :* b :* Nil) = (a, b)
  fromTuple (a, b) = a :* b :* Nil
  toTuple (a :* b :* Nil) = (a, b)

instance TupleLike (a :* b :* c :* Nil) where
  type AsTuple (a :* b :* c :* Nil) = (a, b, c)
  fromTuple (a, b, c) = a :* b :* c :* Nil
  toTuple (a :* b :* c :* Nil) = (a, b, c)

instance TupleLike (a :* b :* c :* d :* Nil) where
  type AsTuple (a :* b :* c :* d :* Nil) = (a, b, c, d)
  fromTuple (a, b, c, d) = a :* b :* c :* d :* Nil
  toTuple (a :* b :* c :* d :* Nil) = (a, b, c, d)

instance TupleLike (a :* b :* c :* d :* e :* Nil) where
  type AsTuple (a :* b :* c :* d :* e :* Nil) = (a, b, c, d, e)
  fromTuple (a, b, c, d, e) = a :* b :* c :* d :* e :* Nil
  toTuple (a :* b :* c :* d :* e :* Nil) = (a, b, c, d, e)

instance TupleLike (a :* b :* c :* d :* e :* f :* Nil) where
  type AsTuple (a :* b :* c :* d :* e :* f :* Nil) = (a, b, c, d, e, f)
  fromTuple (a, b, c, d, e, f) = a :* b :* c :* d :* e :* f :* Nil
  toTuple (a :* b :* c :* d :* e :* f :* Nil) = (a, b, c, d, e, f)

instance TupleLike (a :* b :* c :* d :* e :* f :* g :* Nil) where
  type AsTuple (a :* b :* c :* d :* e :* f :* g :* Nil) = (a, b, c, d, e, f, g)
  fromTuple (a, b, c, d, e, f, g) = a :* b :* c :* d :* e :* f :* g :* Nil
  toTuple (a :* b :* c :* d :* e :* f :* g :* Nil) = (a, b, c, d, e, f, g)

-- | Nullary tuple
data Nil = Nil
  deriving (Eq, Ord, Show)

-- | Tuple constructor
data a :* b = a :* !b
  deriving (Eq, Ord, Show)

infixr 1 :*

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module : Control.Method.Internal
-- License: BSD-3
-- Maintainer: autotaker@gmail.com
-- Stability: experimental
module Control.Method.Internal
  ( TupleLike (..),
    Nil (Nil),
    (:*) ((:*)),
  )
where

import GHC.Generics (Generic)

class TupleLike a where
  type AsTuple a
  fromTuple :: AsTuple a -> a
  toTuple :: a -> AsTuple a

instance TupleLike Nil where
  type AsTuple Nil = ()
  {-# INLINE fromTuple #-}
  fromTuple _ = Nil
  {-# INLINE toTuple #-}
  toTuple _ = ()

instance TupleLike (a :* Nil) where
  type AsTuple (a :* Nil) = a
  {-# INLINE fromTuple #-}
  fromTuple a = a :* Nil
  {-# INLINE toTuple #-}
  toTuple (a :* Nil) = a

instance TupleLike (a :* b :* Nil) where
  type AsTuple (a :* b :* Nil) = (a, b)
  {-# INLINE fromTuple #-}
  fromTuple (a, b) = a :* b :* Nil
  {-# INLINE toTuple #-}
  toTuple (a :* b :* Nil) = (a, b)

instance TupleLike (a :* b :* c :* Nil) where
  type AsTuple (a :* b :* c :* Nil) = (a, b, c)
  {-# INLINE fromTuple #-}
  fromTuple (a, b, c) = a :* b :* c :* Nil
  {-# INLINE toTuple #-}
  toTuple (a :* b :* c :* Nil) = (a, b, c)

instance TupleLike (a :* b :* c :* d :* Nil) where
  type AsTuple (a :* b :* c :* d :* Nil) = (a, b, c, d)
  {-# INLINE fromTuple #-}
  fromTuple (a, b, c, d) = a :* b :* c :* d :* Nil
  {-# INLINE toTuple #-}
  toTuple (a :* b :* c :* d :* Nil) = (a, b, c, d)

instance TupleLike (a :* b :* c :* d :* e :* Nil) where
  type AsTuple (a :* b :* c :* d :* e :* Nil) = (a, b, c, d, e)
  {-# INLINE fromTuple #-}
  fromTuple (a, b, c, d, e) = a :* b :* c :* d :* e :* Nil
  {-# INLINE toTuple #-}
  toTuple (a :* b :* c :* d :* e :* Nil) = (a, b, c, d, e)

instance TupleLike (a :* b :* c :* d :* e :* f :* Nil) where
  type AsTuple (a :* b :* c :* d :* e :* f :* Nil) = (a, b, c, d, e, f)
  {-# INLINE fromTuple #-}
  fromTuple (a, b, c, d, e, f) = a :* b :* c :* d :* e :* f :* Nil
  {-# INLINE toTuple #-}
  toTuple (a :* b :* c :* d :* e :* f :* Nil) = (a, b, c, d, e, f)

instance TupleLike (a :* b :* c :* d :* e :* f :* g :* Nil) where
  type AsTuple (a :* b :* c :* d :* e :* f :* g :* Nil) = (a, b, c, d, e, f, g)
  {-# INLINE fromTuple #-}
  fromTuple (a, b, c, d, e, f, g) = a :* b :* c :* d :* e :* f :* g :* Nil
  {-# INLINE toTuple #-}
  toTuple (a :* b :* c :* d :* e :* f :* g :* Nil) = (a, b, c, d, e, f, g)

-- | Nullary tuple
data Nil = Nil
  deriving (Eq, Ord)

-- | Tuple constructor
data a :* b = a :* !b
  deriving (Eq, Ord, Generic)

instance Show Nil where
  showsPrec _ Nil = showString "()"

instance (Show a, ShowTuple b) => Show (a :* b) where
  showsPrec _ (a :* b) = showsTuple (shows a) b

class ShowTuple a where
  showsTuple :: ShowS -> a -> ShowS

instance ShowTuple Nil where
  showsTuple acc Nil = showParen True acc

instance (Show a, ShowTuple b) => ShowTuple (a :* b) where
  showsTuple acc (a :* b) = showsTuple (acc . showString ", " . shows a) b

infixr 1 :*

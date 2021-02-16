{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Test.Method.Dynamic
  ( DynamicShow,
    castMethod,
    dynArg,
    FromDyn (..),
    ToDyn (..),
    Dynamic,
  )
where

import Control.Method (Method (Args, Base, Ret, curryMethod, uncurryMethod))
import Control.Method.Internal (type (:*))
import Data.Dynamic (Dynamic)
import qualified Data.Dynamic as D
import Data.Typeable (Proxy (Proxy), Typeable, typeRep)
import GHC.Generics
  ( Generic (Rep, from, to),
    K1 (K1),
    M1 (M1),
    U1 (U1),
    type (:*:) ((:*:)),
    type (:+:) (L1, R1),
  )
import Test.Method.Matcher (Matcher)

-- |
-- Module : Test.Method.Dynamic
-- Description:
-- License: BSD-3
-- Maintainer: autotaker@gmail.com
-- Stability: experimental
data DynamicShow = DynamicShow !Dynamic String

instance Show DynamicShow where
  show (DynamicShow v s) = "DynamicShow (" ++ s ++ " :: " ++ show (D.dynTypeRep v) ++ ")"

class FromDyn a b where
  fromDyn :: a -> b
  default fromDyn :: (Generic a, Generic b, FromDyn' (Rep a) (Rep b)) => a -> b
  fromDyn = to . fromDyn' . from

class ToDyn a b where
  toDyn :: b -> a
  default toDyn :: (Generic a, Generic b, ToDyn' (Rep a) (Rep b)) => b -> a
  toDyn = to . toDyn' . from

class FromDyn' f g where
  fromDyn' :: f a -> g a

class ToDyn' f g where
  toDyn' :: g a -> f a

instance (FromDyn' f f', FromDyn' g g') => FromDyn' (f :+: g) (f' :+: g') where
  {-# INLINE fromDyn' #-}
  fromDyn' (L1 a) = L1 (fromDyn' a)
  fromDyn' (R1 b) = R1 (fromDyn' b)

instance (FromDyn' f f', FromDyn' g g') => FromDyn' (f :*: g) (f' :*: g') where
  {-# INLINE fromDyn' #-}
  fromDyn' (a :*: b) = fromDyn' a :*: fromDyn' b

instance (FromDyn a a') => FromDyn' (K1 i a) (K1 i a') where
  {-# INLINE fromDyn' #-}
  fromDyn' (K1 a) = K1 (fromDyn a)

instance FromDyn' U1 U1 where
  {-# INLINE fromDyn' #-}
  fromDyn' _ = U1

instance (FromDyn' f f') => FromDyn' (M1 i t f) (M1 i t f') where
  {-# INLINE fromDyn' #-}
  fromDyn' (M1 a) = M1 (fromDyn' a)

instance Typeable a => FromDyn Dynamic a where
  fromDyn = fromDynamic

instance (Typeable a, Show a) => FromDyn DynamicShow a where
  fromDyn = fromDynamic

instance {-# INCOHERENT #-} FromDyn a a where
  {-# INLINE fromDyn #-}
  fromDyn = id

instance (ToDyn' f f', ToDyn' g g') => ToDyn' (f :+: g) (f' :+: g') where
  {-# INLINE toDyn' #-}
  toDyn' (L1 a) = L1 (toDyn' a)
  toDyn' (R1 b) = R1 (toDyn' b)

instance (ToDyn' f f', ToDyn' g g') => ToDyn' (f :*: g) (f' :*: g') where
  {-# INLINE toDyn' #-}
  toDyn' (a :*: b) = toDyn' a :*: toDyn' b

instance (ToDyn a a') => ToDyn' (K1 i a) (K1 i a') where
  {-# INLINE toDyn' #-}
  toDyn' (K1 a) = K1 (toDyn a)

instance ToDyn' U1 U1 where
  {-# INLINE toDyn' #-}
  toDyn' _ = U1

instance (ToDyn' f f') => ToDyn' (M1 i t f) (M1 i t f') where
  {-# INLINE toDyn' #-}
  toDyn' (M1 a) = M1 (toDyn' a)

instance Typeable a => ToDyn Dynamic a where
  toDyn = D.toDyn

instance (Typeable a, Show a) => ToDyn DynamicShow a where
  toDyn = toDynamicShow

instance {-# INCOHERENT #-} ToDyn a a where
  {-# INLINE toDyn #-}
  toDyn = id

instance (FromDyn a b, FromDyn c d) => FromDyn (a :* c) (b :* d)

instance (ToDyn a b, ToDyn c d) => ToDyn (a :* c) (b :* d)

instance (FromDyn a b) => FromDyn [a] [b]

instance (ToDyn a b) => ToDyn [a] [b]

instance (FromDyn a b) => FromDyn (Maybe a) (Maybe b)

instance (ToDyn a b) => ToDyn (Maybe a) (Maybe b)

instance (FromDyn a a', FromDyn b b') => FromDyn (a, b) (a', b')

instance (ToDyn a a', ToDyn b b') => ToDyn (a, b) (a', b')

instance (FromDyn a a', FromDyn b b', FromDyn c c') => FromDyn (a, b, c) (a', b', c')

instance (ToDyn a a', ToDyn b b', ToDyn c c') => ToDyn (a, b, c) (a', b', c')

instance (FromDyn a a', FromDyn b b', FromDyn c c', FromDyn d d') => FromDyn (a, b, c, d) (a', b', c', d')

instance (ToDyn a a', ToDyn b b', ToDyn c c', ToDyn d d') => ToDyn (a, b, c, d) (a', b', c', d')

instance (FromDyn a a', FromDyn b b', FromDyn c c', FromDyn d d', FromDyn e e') => FromDyn (a, b, c, d, e) (a', b', c', d', e')

instance (ToDyn a a', ToDyn b b', ToDyn c c', ToDyn d d', ToDyn e e') => ToDyn (a, b, c, d, e) (a', b', c', d', e')

instance (FromDyn a a', FromDyn b b', FromDyn c c', FromDyn d d', FromDyn e e', FromDyn f f') => FromDyn (a, b, c, d, e, f) (a', b', c', d', e', f')

instance (ToDyn a a', ToDyn b b', ToDyn c c', ToDyn d d', ToDyn e e', ToDyn f f') => ToDyn (a, b, c, d, e, f) (a', b', c', d', e', f')

instance (FromDyn a a', FromDyn b b', FromDyn c c', FromDyn d d', FromDyn e e', FromDyn f f', FromDyn g g') => FromDyn (a, b, c, d, e, f, g) (a', b', c', d', e', f', g')

instance (ToDyn a a', ToDyn b b', ToDyn c c', ToDyn d d', ToDyn e e', ToDyn f f', ToDyn g g') => ToDyn (a, b, c, d, e, f, g) (a', b', c', d', e', f', g')

castMethod ::
  ( ToDyn (Args method) (Args method'),
    FromDyn (Ret method) (Ret method'),
    Method method,
    Method method',
    Base method ~ Base method'
  ) =>
  method ->
  method'
castMethod method = curryMethod $ \args ->
  fromDyn <$> uncurryMethod method (toDyn args)

fromDynamic :: forall a d. (Typeable a, DynamicLike d, Show d) => d -> a
fromDynamic v =
  D.fromDyn (asDyn v) $
    error $ "cannot cast " ++ show v ++ " to " ++ show (typeRep (Proxy :: Proxy a))

toDynamicShow :: (Typeable a, Show a) => a -> DynamicShow
toDynamicShow a = DynamicShow (D.toDyn a) (show a)

class DynamicLike a where
  asDyn :: a -> Dynamic

instance DynamicLike Dynamic where
  asDyn = id

instance DynamicLike DynamicShow where
  asDyn (DynamicShow a _) = a

dynArg :: (Typeable a, DynamicLike b) => Matcher a -> Matcher b
dynArg matcher dv =
  maybe False matcher $ D.fromDynamic $ asDyn dv
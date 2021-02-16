{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Test.Method.Dynamic where

import Control.Method (Method (Args, Base, Ret, curryMethod, uncurryMethod))
import Control.Method.Internal (type (:*) ((:*)))
import Data.Dynamic
  ( Dynamic,
    Typeable,
    dynTypeRep,
    fromDyn,
    fromDynamic,
    toDyn,
  )
import Data.Typeable (Proxy (Proxy), typeRep)
import GHC.Generics
import Test.Method.Matcher (Matcher)

-- |
-- Module : Test.Method.Dynamic
-- Description:
-- License: BSD-3
-- Maintainer: autotaker@gmail.com
-- Stability: experimental
data DynamicShow = DynamicShow !Dynamic String

instance Show DynamicShow where
  show (DynamicShow v s) = "DynamicShow (" ++ s ++ " :: " ++ show (dynTypeRep v) ++ ")"

class DCastable a b where
  castFromDynamic :: a -> b
  default castFromDynamic :: (Generic a, Generic b, DCastable' (Rep a) (Rep b)) => a -> b
  castFromDynamic a = to (castFromDynamic' (from a))
  castToDynamic :: b -> a
  default castToDynamic :: (Generic a, Generic b, DCastable' (Rep a) (Rep b)) => b -> a
  castToDynamic a = to (castToDynamic' (from a))

class DCastable' f g where
  castFromDynamic' :: f a -> g a
  castToDynamic' :: g a -> f a

instance (DCastable' f f', DCastable' g g') => DCastable' (f :+: g) (f' :+: g') where
  castFromDynamic' (L1 a) = L1 (castFromDynamic' a)
  castFromDynamic' (R1 b) = R1 (castFromDynamic' b)
  castToDynamic' (L1 a) = L1 (castToDynamic' a)
  castToDynamic' (R1 b) = R1 (castToDynamic' b)

instance (DCastable' f f', DCastable' g g') => DCastable' (f :*: g) (f' :*: g') where
  castFromDynamic' (a :*: b) = castFromDynamic' a :*: castFromDynamic' b
  castToDynamic' (a :*: b) = castToDynamic' a :*: castToDynamic' b

instance (DCastable a a') => DCastable' (K1 i a) (K1 i a') where
  castFromDynamic' (K1 a) = K1 (castFromDynamic a)
  castToDynamic' (K1 a) = K1 (castToDynamic a)

instance DCastable' U1 U1 where
  castFromDynamic' _ = U1
  castToDynamic' _ = U1

instance (DCastable' f f') => DCastable' (M1 i t f) (M1 i t f') where
  castFromDynamic' (M1 a) = M1 (castFromDynamic' a)
  castToDynamic' (M1 a) = M1 (castToDynamic' a)

instance Typeable a => DCastable Dynamic a where
  castFromDynamic = (`fromDyn` error "cannot cast ")
  castToDynamic = toDyn

instance (Typeable a, Show a) => DCastable DynamicShow a where
  castFromDynamic = fromDynamicShow
  castToDynamic a = DynamicShow (toDyn a) (show a)

instance {-# INCOHERENT #-} DCastable a a where
  castFromDynamic = id
  castToDynamic = id

instance (DCastable a b, DCastable c d) => DCastable (a :* c) (b :* d) where
  castFromDynamic (a :* b) = castFromDynamic a :* castFromDynamic b
  castToDynamic (c :* d) = castToDynamic c :* castToDynamic d

instance (DCastable a b) => DCastable [a] [b]

instance (DCastable a b) => DCastable (Maybe a) (Maybe b)

instance (DCastable a a', DCastable b b') => DCastable (a, b) (a', b')

instance (DCastable a a', DCastable b b', DCastable c c') => DCastable (a, b, c) (a', b', c')

instance (DCastable a a', DCastable b b', DCastable c c', DCastable d d') => DCastable (a, b, c, d) (a', b', c', d')

instance (DCastable a a', DCastable b b', DCastable c c', DCastable d d', DCastable e e') => DCastable (a, b, c, d, e) (a', b', c', d', e')

instance (DCastable a a', DCastable b b', DCastable c c', DCastable d d', DCastable e e', DCastable f f') => DCastable (a, b, c, d, e, f) (a', b', c', d', e', f')

instance (DCastable a a', DCastable b b', DCastable c c', DCastable d d', DCastable e e', DCastable f f', DCastable g g') => DCastable (a, b, c, d, e, f, g) (a', b', c', d', e', f', g')

castMethod ::
  ( DCastable (Args method) (Args method'),
    DCastable (Ret method) (Ret method'),
    Method method,
    Method method',
    Base method ~ Base method'
  ) =>
  method ->
  method'
castMethod method = curryMethod $ \args ->
  castFromDynamic <$> uncurryMethod method (castToDynamic args)

fromDynamicShow :: forall a. Typeable a => DynamicShow -> a
fromDynamicShow v =
  fromDyn (asDyn v) $
    error $ "cannot cast " ++ show v ++ " to " ++ show (typeRep (Proxy :: Proxy a))

toDynShow :: (Typeable a, Show a) => a -> DynamicShow
toDynShow a = DynamicShow (toDyn a) (show a)

class DynamicLike a where
  asDyn :: a -> Dynamic

instance DynamicLike Dynamic where
  asDyn = id

instance DynamicLike DynamicShow where
  asDyn (DynamicShow a _) = a

dynArg :: (Typeable a, DynamicLike b) => Matcher a -> Matcher b
dynArg matcher dv =
  maybe False matcher $ fromDynamic $ asDyn dv
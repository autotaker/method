{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Test.Method.Matcher where

import Control.Method (Nil (Nil), (:*) ((:*)))

type Matcher a = a -> Bool

anything :: Matcher a
anything = const True

class ArgsTuple a where
  type EachMatcher a
  type AsTuple a
  args :: EachMatcher a -> Matcher a
  args' :: Matcher (AsTuple a) -> Matcher a

instance ArgsTuple Nil where
  type EachMatcher Nil = ()
  type AsTuple Nil = ()
  args _ = anything
  args' matcher Nil = matcher ()

instance ArgsTuple (a :* Nil) where
  type EachMatcher (a :* Nil) = Matcher a
  type AsTuple (a :* Nil) = a
  args matcher (a :* Nil) = matcher a
  args' matcher (a :* Nil) = matcher a

instance ArgsTuple (a :* b :* Nil) where
  type EachMatcher (a :* b :* Nil) = (Matcher a, Matcher b)
  type AsTuple (a :* b :* Nil) = (a, b)
  args (ma, mb) (a :* b :* Nil) = ma a && mb b
  args' matcher (a :* b :* Nil) = matcher (a, b)

instance ArgsTuple (a :* b :* c :* Nil) where
  type EachMatcher (a :* b :* c :* Nil) = (Matcher a, Matcher b, Matcher c)
  type AsTuple (a :* b :* c :* Nil) = (a, b, c)
  args (ma, mb, mc) (a :* b :* c :* Nil) = ma a && mb b && mc c
  args' matcher (a :* b :* c :* Nil) = matcher (a, b, c)

instance ArgsTuple (a :* b :* c :* d :* Nil) where
  type EachMatcher (a :* b :* c :* d :* Nil) = (Matcher a, Matcher b, Matcher c, Matcher d)
  type AsTuple (a :* b :* c :* d :* Nil) = (a, b, c, d)
  args (ma, mb, mc, md) (a :* b :* c :* d :* Nil) = ma a && mb b && mc c && md d
  args' matcher (a :* b :* c :* d :* Nil) = matcher (a, b, c, d)

instance ArgsTuple (a :* b :* c :* d :* e :* Nil) where
  type EachMatcher (a :* b :* c :* d :* e :* Nil) = (Matcher a, Matcher b, Matcher c, Matcher d, Matcher e)
  type AsTuple (a :* b :* c :* d :* e :* Nil) = (a, b, c, d, e)
  args (ma, mb, mc, md, me) (a :* b :* c :* d :* e :* Nil) = ma a && mb b && mc c && md d && me e
  args' matcher (a :* b :* c :* d :* e :* Nil) = matcher (a, b, c, d, e)

instance ArgsTuple (a :* b :* c :* d :* e :* f :* Nil) where
  type EachMatcher (a :* b :* c :* d :* e :* f :* Nil) = (Matcher a, Matcher b, Matcher c, Matcher d, Matcher e, Matcher f)
  type AsTuple (a :* b :* c :* d :* e :* f :* Nil) = (a, b, c, d, e, f)
  args (ma, mb, mc, md, me, mf) (a :* b :* c :* d :* e :* f :* Nil) = ma a && mb b && mc c && md d && me e && mf f
  args' matcher (a :* b :* c :* d :* e :* f :* Nil) = matcher (a, b, c, d, e, f)

instance ArgsTuple (a :* b :* c :* d :* e :* f :* g :* Nil) where
  type EachMatcher (a :* b :* c :* d :* e :* f :* g :* Nil) = (Matcher a, Matcher b, Matcher c, Matcher d, Matcher e, Matcher f, Matcher g)
  type AsTuple (a :* b :* c :* d :* e :* f :* g :* Nil) = (a, b, c, d, e, f, g)
  args (ma, mb, mc, md, me, mf, mg) (a :* b :* c :* d :* e :* f :* g :* Nil) = ma a && mb b && mc c && md d && me e && mf f && mg g
  args' matcher (a :* b :* c :* d :* e :* f :* g :* Nil) = matcher (a, b, c, d, e, f, g)

when :: Matcher a -> Matcher a
when = id
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Test.Method.Matcher
  ( anything,
    when,
    Matcher,
    TupleLike (..),
    ArgsMatcher (..),
  )
where

import Control.Method.Internal (Nil (Nil), TupleLike (AsTuple), (:*) ((:*)))

type Matcher a = a -> Bool

anything :: Matcher a
anything = const True

when :: Matcher a -> Matcher a
when = id

class TupleLike a => ArgsMatcher a where
  type EachMatcher a
  args :: EachMatcher a -> Matcher a
  args' :: Matcher (AsTuple a) -> Matcher a

instance ArgsMatcher Nil where
  type EachMatcher Nil = ()
  args _ = anything
  args' matcher Nil = matcher ()

instance ArgsMatcher (a :* Nil) where
  type EachMatcher (a :* Nil) = Matcher a
  args matcher (a :* Nil) = matcher a
  args' matcher (a :* Nil) = matcher a

instance ArgsMatcher (a :* b :* Nil) where
  type EachMatcher (a :* b :* Nil) = (Matcher a, Matcher b)
  args (ma, mb) (a :* b :* Nil) = ma a && mb b
  args' matcher (a :* b :* Nil) = matcher (a, b)

instance ArgsMatcher (a :* b :* c :* Nil) where
  type EachMatcher (a :* b :* c :* Nil) = (Matcher a, Matcher b, Matcher c)
  args (ma, mb, mc) (a :* b :* c :* Nil) = ma a && mb b && mc c
  args' matcher (a :* b :* c :* Nil) = matcher (a, b, c)

instance ArgsMatcher (a :* b :* c :* d :* Nil) where
  type EachMatcher (a :* b :* c :* d :* Nil) = (Matcher a, Matcher b, Matcher c, Matcher d)
  args (ma, mb, mc, md) (a :* b :* c :* d :* Nil) = ma a && mb b && mc c && md d
  args' matcher (a :* b :* c :* d :* Nil) = matcher (a, b, c, d)

instance ArgsMatcher (a :* b :* c :* d :* e :* Nil) where
  type EachMatcher (a :* b :* c :* d :* e :* Nil) = (Matcher a, Matcher b, Matcher c, Matcher d, Matcher e)
  args (ma, mb, mc, md, me) (a :* b :* c :* d :* e :* Nil) = ma a && mb b && mc c && md d && me e
  args' matcher (a :* b :* c :* d :* e :* Nil) = matcher (a, b, c, d, e)

instance ArgsMatcher (a :* b :* c :* d :* e :* f :* Nil) where
  type EachMatcher (a :* b :* c :* d :* e :* f :* Nil) = (Matcher a, Matcher b, Matcher c, Matcher d, Matcher e, Matcher f)
  args (ma, mb, mc, md, me, mf) (a :* b :* c :* d :* e :* f :* Nil) = ma a && mb b && mc c && md d && me e && mf f
  args' matcher (a :* b :* c :* d :* e :* f :* Nil) = matcher (a, b, c, d, e, f)

instance ArgsMatcher (a :* b :* c :* d :* e :* f :* g :* Nil) where
  type EachMatcher (a :* b :* c :* d :* e :* f :* g :* Nil) = (Matcher a, Matcher b, Matcher c, Matcher d, Matcher e, Matcher f, Matcher g)
  args (ma, mb, mc, md, me, mf, mg) (a :* b :* c :* d :* e :* f :* g :* Nil) = ma a && mb b && mc c && md d && me e && mf f && mg g
  args' matcher (a :* b :* c :* d :* e :* f :* g :* Nil) = matcher (a, b, c, d, e, f, g)

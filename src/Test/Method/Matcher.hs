{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Test.Method.Matcher
  ( anything,
    when,
    Matcher,
    TupleLike (..),
    ArgsMatcher (..),
    args',
  )
where

import Control.Method.Internal (Nil (Nil), TupleLike (AsTuple, fromTuple, toTuple), (:*) ((:*)))

type Matcher a = a -> Bool

-- | Matcher that matches anything
anything :: Matcher a
anything = const True

-- | synonym of 'id' function.
-- Use this function for improving readability
when :: Matcher a -> Matcher a
when = id

-- | Matcher for 'Control.Method.Args'
--
-- >>> args ((==2), (>3)) (2 :* 4 :* Nil)
-- True
-- >>> args even (1 :* Nil)
-- False
-- >>> args () Nil
-- True
class TupleLike a => ArgsMatcher a where
  type EachMatcher a

  -- | Convert a tuple of matchers to a matcher of tuples
  args :: EachMatcher a -> Matcher a

-- | Convert a tuple matcher to a tuple-like matcher.
--
-- >>> args' (\(a, b) -> a * b == 10) (2 :* 5 :* Nil)
-- True
-- >>> args' (\(a, b) -> a * b == 10) (2 :* 4 :* Nil)
-- False
args' :: TupleLike a => Matcher (AsTuple a) -> Matcher a
args' m a = m (toTuple a)

instance ArgsMatcher Nil where
  type EachMatcher Nil = ()
  args _ = anything

instance ArgsMatcher (a :* Nil) where
  type EachMatcher (a :* Nil) = Matcher a
  args matcher (a :* Nil) = matcher a

instance ArgsMatcher (a :* b :* Nil) where
  type EachMatcher (a :* b :* Nil) = (Matcher a, Matcher b)
  args (ma, mb) (a :* b :* Nil) = ma a && mb b

instance ArgsMatcher (a :* b :* c :* Nil) where
  type EachMatcher (a :* b :* c :* Nil) = (Matcher a, Matcher b, Matcher c)
  args (ma, mb, mc) (a :* b :* c :* Nil) = ma a && mb b && mc c

instance ArgsMatcher (a :* b :* c :* d :* Nil) where
  type EachMatcher (a :* b :* c :* d :* Nil) = (Matcher a, Matcher b, Matcher c, Matcher d)
  args (ma, mb, mc, md) (a :* b :* c :* d :* Nil) = ma a && mb b && mc c && md d

instance ArgsMatcher (a :* b :* c :* d :* e :* Nil) where
  type EachMatcher (a :* b :* c :* d :* e :* Nil) = (Matcher a, Matcher b, Matcher c, Matcher d, Matcher e)
  args (ma, mb, mc, md, me) (a :* b :* c :* d :* e :* Nil) = ma a && mb b && mc c && md d && me e

instance ArgsMatcher (a :* b :* c :* d :* e :* f :* Nil) where
  type EachMatcher (a :* b :* c :* d :* e :* f :* Nil) = (Matcher a, Matcher b, Matcher c, Matcher d, Matcher e, Matcher f)
  args (ma, mb, mc, md, me, mf) (a :* b :* c :* d :* e :* f :* Nil) = ma a && mb b && mc c && md d && me e && mf f

instance ArgsMatcher (a :* b :* c :* d :* e :* f :* g :* Nil) where
  type EachMatcher (a :* b :* c :* d :* e :* f :* g :* Nil) = (Matcher a, Matcher b, Matcher c, Matcher d, Matcher e, Matcher f, Matcher g)
  args (ma, mb, mc, md, me, mf, mg) (a :* b :* c :* d :* e :* f :* g :* Nil) = ma a && mb b && mc c && md d && me e && mf f && mg g

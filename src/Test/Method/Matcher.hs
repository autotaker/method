{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Test.Method.Matcher where

import Control.Method (Nil (Nil), (:*) ((:*)))

type Matcher a = a -> Bool

anything :: Matcher a
anything = const True

class ToMatcher a where
  type MatchAs a
  when :: a -> Matcher (MatchAs a)

instance ToMatcher (Matcher a) where
  type MatchAs (Matcher a) = a :* Nil
  when ma (a :* Nil) = ma a

instance ToMatcher (Matcher a, Matcher b) where
  type MatchAs (Matcher a, Matcher b) = a :* b :* Nil
  when (ma, mb) (a :* b :* Nil) = ma a && mb b

instance ToMatcher (Matcher a, Matcher b, Matcher c) where
  type MatchAs (Matcher a, Matcher b, Matcher c) = a :* b :* c :* Nil
  when (ma, mb, mc) (a :* b :* c :* Nil) = ma a && mb b && mc c

instance ToMatcher (Matcher a, Matcher b, Matcher c, Matcher d) where
  type MatchAs (Matcher a, Matcher b, Matcher c, Matcher d) = a :* b :* c :* d :* Nil
  when (ma, mb, mc, md) (a :* b :* c :* d :* Nil) = ma a && mb b && mc c && md d

instance ToMatcher (Matcher a, Matcher b, Matcher c, Matcher d, Matcher e) where
  type MatchAs (Matcher a, Matcher b, Matcher c, Matcher d, Matcher e) = a :* b :* c :* d :* e :* Nil
  when (ma, mb, mc, md, me) (a :* b :* c :* d :* e :* Nil) = ma a && mb b && mc c && md d && me e

instance ToMatcher (Matcher a, Matcher b, Matcher c, Matcher d, Matcher e, Matcher f) where
  type MatchAs (Matcher a, Matcher b, Matcher c, Matcher d, Matcher e, Matcher f) = a :* b :* c :* d :* e :* f :* Nil
  when (ma, mb, mc, md, me, mf) (a :* b :* c :* d :* e :* f :* Nil) = ma a && mb b && mc c && md d && me e && mf f
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Method.Behavior
  ( Behave (..),
    thenReturn,
    thenAction,
  )
where

import Control.Method (Method (Base, Ret, curryMethod))

class Behave x where
  type LHS x
  type MethodOf x
  thenMethod :: LHS x -> MethodOf x -> x

thenReturn ::
  (Behave x, Method (MethodOf x)) =>
  LHS x ->
  Ret (MethodOf x) ->
  x
thenReturn lhs v =
  thenMethod lhs $ curryMethod $ const $ pure v

thenAction ::
  (Behave x, Method (MethodOf x)) =>
  LHS x ->
  Base (MethodOf x) (Ret (MethodOf x)) ->
  x
thenAction lhs action =
  thenMethod lhs $ curryMethod $ const action

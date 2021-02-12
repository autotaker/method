{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Method.Behavior
  ( Behave (..),
    thenReturn,
    thenAction,
  )
where

import Control.Method (Method (Base, Ret, curryMethod))

-- | A type class whose behavior is specified by a method
class Behave x where
  -- | Type of the first argument of 'thenMethod',
  --   representing the condition when the method is called
  type Condition x

  -- | Type of the second argument of 'thenMethod',
  --   representing a method to be called.
  type MethodOf x

  -- | Specify behavior from a pair of a condition and a method.
  thenMethod :: Condition x -> MethodOf x -> x

-- | Specify behavior that return a constant value for a call
thenReturn ::
  (Behave x, Method (MethodOf x)) =>
  Condition x ->
  Ret (MethodOf x) ->
  x
thenReturn lhs v =
  thenMethod lhs $ curryMethod $ const $ pure v

-- | Specify behavior that executes an action for a call
thenAction ::
  (Behave x, Method (MethodOf x)) =>
  Condition x ->
  Base (MethodOf x) (Ret (MethodOf x)) ->
  x
thenAction lhs action =
  thenMethod lhs $ curryMethod $ const action

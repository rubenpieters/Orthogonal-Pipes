{-# LANGUAGE RankNTypes #-}

module OrthPipes.Proxy (
    ReS(..)

  , Proxy(..)
  , (>++>)
  , (++>>)

  , runEffectPr
  , fetchResponsesPr
  , foldResponsesPr
  ) where

import Data.Void

import Unsafe.Coerce

-- restricted Scott encoding of request/respond
newtype ReS i o a = ReS
  { unReS :: (o -> (i -> ReS i o a) -> a) -> a
  }

-- orthogonal encoding of pipes
newtype Proxy a' a b' b m = Proxy
  { unProxy :: forall r.
      (a' -> (a -> ReS a a' (m r)) -> m r) -> -- request
      (b -> (b' -> ReS b' b (m r)) -> m r) -> -- respond
      m r ->                              -- exit
      m r
  }

-- merge helper functions

mergeLProxy ::
  (a' -> (a -> ReS a a' (m r)) -> m r) ->
  m r ->
  Proxy a' a b' b m ->
  ReS b' b (m r)
mergeLProxy req e p = ReS (\res -> unProxy p req res e)

mergeRProxy ::
  (b -> (b' -> ReS b' b (m r)) -> m r) ->
  m r ->
  Proxy a' a b' b m ->
  ReS a a' (m r)
mergeRProxy res e p = ReS (\req -> unProxy p req res e)

mergeReS :: ReS i o a -> (o -> ReS o i a) -> a
mergeReS = unsafeCoerce
-- less efficient implementation
--mergeReS (ReS res) f = res (mergeReS . f)

-- efficient orthogonal pipes merge
(++>>) ::
  (b' -> Proxy a' a b' b m) ->
  Proxy b' b c' c m ->
  Proxy a' a c' c m
fp ++>> q = Proxy (\req res e ->
    mergeReS (mergeRProxy res e q) (mergeLProxy req e . fp)
  )

-- equivalent to request a' >>= respond
cat :: a' -> Proxy a' a a' a m
cat a' = Proxy (\req res e ->
  req a' (\a -> ReS (\req' ->
  res a (\b' -> ReS (\res' ->
  unProxy (cat b') req' res' e
  )))))

-- TODO: check if `cat` is the identity element of `>++>`
(>++>) ::
  ( b' -> Proxy a' a b' b m) ->
  (_c' -> Proxy b' b c' c m) ->
  (_c' -> Proxy a' a c' c m)
(fb' >++> fc') c' = fb' ++>> fc' c'

-- run all effects
runEffectPr :: (Monad m) => Proxy Void () () Void m -> m ()
runEffectPr = foldResponsesPr (\_ _ -> ()) ()

-- fetch all output values into a list
fetchResponsesPr :: (Monad m) => Proxy x () () o m -> m [o]
fetchResponsesPr = foldResponsesPr (flip (:)) []

-- fold over all output values
foldResponsesPr :: (Monad m) => (b -> o -> b) -> b -> Proxy x () () o m -> m b
foldResponsesPr combine init (Proxy proxy) = proxy
  (\v f -> foldResponses'' (f ()))
  (\o f -> (`combine` o) <$> foldResponses' combine (f ()))
  (return init)
  where
    foldResponses' ::
      (Functor m) => (b -> o -> b) -> ReS () o (m b) -> m b
    foldResponses' combine (ReS r) =
      r (\o f -> (`combine` o) <$> foldResponses' combine (f ()))
    foldResponses'' ::
      (Functor m) => ReS () x (m b) -> m b
    foldResponses'' (ReS r) =
      r (\b f -> foldResponses'' (f ()))


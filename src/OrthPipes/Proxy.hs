{-# LANGUAGE RankNTypes #-}

module OrthPipes.Proxy (
    ReS(..)
  , MS(..)

  , Proxy(..)
  , (>+>)
  , (+>>)

  , runEffect
  , fetchResponses
  , foldResponses
  ) where

import Data.Void

import Unsafe.Coerce

newtype ReS i o a = ReS
  { unReS :: (o -> (i -> ReS i o a) -> a) -> a
  }

newtype MS m a = MS
  { unMS :: (m (MS m a) -> a) -> a
  }

newtype Proxy a' a b' b m = Proxy
  { unProxy :: forall r.
      (a' -> (a -> ReS a a' r) -> r) -> -- request
      (b -> (b' -> ReS b' b r) -> r) -> -- respond
      (m (MS m r) -> r) ->              -- lift m
      r ->                              -- exit
      r
  }

mergeLProxy ::
  (a' -> (a -> ReS a a' r) -> r) ->
  (m (MS m r) -> r) ->
  r ->
  Proxy a' a b' b m ->
  ReS b' b r
mergeLProxy req m e p = ReS (\res -> unProxy p req res m e)

mergeRProxy ::
  (b -> (b' -> ReS b' b r) -> r) ->
  (m (MS m r) -> r) ->
  r ->
  Proxy a' a b' b m ->
  ReS a a' r
mergeRProxy res m e p = ReS (\req -> unProxy p req res m e)

mergeReS :: ReS i o a -> (o -> ReS o i a) -> a
mergeReS = unsafeCoerce

-- less efficient implementation
--mergeReS (ReS res) f = res (mergeReS . f)

(+>>) ::
  (b' -> Proxy a' a b' b m) ->
  Proxy b' b c' c m ->
  Proxy a' a c' c m
fp +>> q = Proxy (\req res m e ->
    mergeReS (mergeRProxy res m e q) (mergeLProxy req m e . fp)
  )

(>+>) ::
  Functor m =>
  ( b' -> Proxy a' a b' b m) ->
  (_c' -> Proxy b' b c' c m) ->
  (_c' -> Proxy a' a c' c m)
(fb' >+> fc') c' = fb' +>> fc' c'

runEffect :: (Monad m) => Proxy Void () () Void m -> m ()
runEffect (Proxy proxy) = proxy
  (\v _ -> absurd v)
  (\v _ -> absurd v)
  fixMMS
  (return ())

fixMMS :: (Monad m) => m (MS m (m a)) -> m a
fixMMS h = do
  h' <- h
  unMS h' fixMMS

fetchResponses :: (Monad m) => Proxy x () () o m -> m [o]
fetchResponses = foldResponses (flip (:)) []

foldResponses :: (Monad m) => (b -> o -> b) -> b -> Proxy x () () o m -> m b
foldResponses combine init (Proxy proxy) = proxy
  (\v f -> foldResponses'' (f ()))
  (\o f -> (`combine` o) <$> foldResponses' combine (f ()))
  fixMMS
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


{-# LANGUAGE RankNTypes #-}

module Pipes.Orth.Core (
    Proxy
  , runEffect

  , respond

  , request

  -- ** Pull
  -- $pull
  , pull
  , (>+>)
  , (+>>)


  -- * Concrete Type Synonyms
  , X
  , Effect
  , Producer
  , Pipe
  , Consumer
  , Client
  , Server

  -- * Polymorphic Type Synonyms
  , Effect'
  , Producer'
  , Consumer'
  , Client'
  , Server'

  ) where

import Pipes.Orth.Internal

import Unsafe.Coerce

runEffect :: (Monad m) => Effect m r -> m r
runEffect  p = unOrthProxy (unProxy p (\r -> OrthProxy (\_ _ _ _ -> return r)))
  (\v _ -> closed v)
  (\v _ -> closed v)
  fixMMS
  (return (error "runEffect exit"))

fixMMS :: (Monad m) => m (MS m (m a)) -> m a
fixMMS h = do
  h' <- h
  unMS h' fixMMS

respond :: a -> Proxy x' x a' a m a'
respond a = Proxy (\k ->
  OrthProxy (\req res m e -> res a (\x ->
    ReS (\res' -> unOrthProxy (k x) req res' m e))
  ))

request :: a' -> Proxy a' a y' y m a
request a' = Proxy (\k ->
  OrthProxy (\req res m e -> req a' (\x ->
    ReS (\req' -> unOrthProxy (k x) req' res m e))
  ))

pull :: a' -> Proxy a' a a' a m r
pull a' = Proxy (\_ -> OrthProxy (\req res _ _ ->
    req a' (\a -> ReS (\req' ->
    res a (go req' res)
  ))))
  where
    go req res x' = ReS (\res' ->
        req x' (\x -> ReS (\req' ->
        res x (go req' res')
      )))

(>+>)
    :: Functor m
    => ( b' -> Proxy a' a b' b m r)
    -> (_c' -> Proxy b' b c' c m r)
    -> (_c' -> Proxy a' a c' c m r)
(fb' >+> fc') c' = fb' +>> fc' c'

(+>>) ::
  (b' -> Proxy a' a b' b m r) ->
  Proxy b' b c' c m r ->
  Proxy a' a c' c m r
fp +>> q =
  Proxy (\_ -> mergeProxy
    ((\p -> unProxy p (\_ -> OrthProxy (\_ _ _ e -> e))) . fp)
    (unProxy q (\_ -> OrthProxy (\_ _ _ e -> e)))
  )

mergeLProxy ::
  (a' -> (a -> ReS a a' r) -> r) ->
  (m (MS m r) -> r) ->
  r ->
  OrthProxy a' a b' b m r ->
  ReS b' b r
mergeLProxy req m e p = ReS (\res -> unOrthProxy p req res m e)

mergeRProxy ::
  (b -> (b' -> ReS b' b r) -> r) ->
  (m (MS m r) -> r) ->
  r ->
  OrthProxy a' a b' b m r -> ReS a a' r
mergeRProxy res m e p = ReS (\req -> unOrthProxy p req res m e)

mergeProxy ::
  (b' -> OrthProxy a' a b' b m r) ->
  OrthProxy b' b c' c m r ->
  OrthProxy a' a c' c m r
mergeProxy fp q = OrthProxy (\req res m e ->
    mergeReS (mergeRProxy res m e q) (mergeLProxy req m e . fp)
  )

mergeReS :: ReS i o a -> (o -> ReS o i a) -> a
mergeReS = unsafeCoerce
--mergeReS (ReS res) f = res (mergeReS . f)


type Effect = Proxy X () () X
type Producer b = Proxy X () () b
type Pipe a b = Proxy () a () b
type Consumer a = Proxy () a () X
type Client a' a = Proxy a' a () X
type Server b' b = Proxy X () b' b
type Effect' m r = forall x' x y' y . Proxy x' x y' y m r
type Producer' b m r = forall x' x . Proxy x' x () b m r
type Consumer' a m r = forall y' y . Proxy () a y' y m r
type Server' b' b m r = forall x' x . Proxy x' x b' b m r
type Client' a' a m r = forall y' y . Proxy a' a y' y m r

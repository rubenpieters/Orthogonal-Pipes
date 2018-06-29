module Pipes.Orth.Core (
    Proxy

  , respond

  , request

  , (+>>)
  ) where

import Pipes.Orth.Internal

import Unsafe.Coerce

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

(+>>) ::
  (b' -> Proxy a' a b' b m r) ->
  Proxy b' b c' c m r ->
  Proxy a' a c' c m r
fp +>> q =
  Proxy (\_ -> mergeProxy
    ((\p -> unProxy p (\_ -> OrthProxy (\_ _ _ e -> e))) . fp)
    (unProxy q (\_ -> OrthProxy (\_ _ _ e -> e)))
  )

{-# LANGUAGE RankNTypes #-}

module OrthPipes.Plan (
    Plan(..)
  , construct
  , repeatedly
  , deconstruct

  , respond
  , request
  , await
  , yield
  , exit
  ) where

import OrthPipes.Proxy

import Control.Monad
import Control.Monad.Trans.Class (MonadTrans (lift))

newtype Plan a' a b' b m x = Plan
  { unPlan ::
      (x -> Proxy a' a b' b m) -> Proxy a' a b' b m
  }

instance Functor (Plan a' a b' b m) where
  fmap = liftM

instance Applicative (Plan a' a b' b m) where
  pure = return
  (<*>) = ap

instance Monad (Plan a' a b' b m) where
  return x = Plan (\k -> k x)
  p >>= f = Plan (\k -> unPlan p (\x -> unPlan (f x) k))

instance MonadTrans (Plan a' a b' b) where
    lift mr = Plan (\k ->
      Proxy (\req res m e ->
        m (fmap ((\proxy -> MS (\m' -> unProxy proxy req res m' e)) . k) mr)
      ))

construct :: Plan a' a b' b m x -> Proxy a' a b' b m
construct (Plan plan) = plan (\_ -> Proxy (\_ _ _ e -> e))

repeatedly :: Plan a' a b' b m x -> Proxy a' a b' b m
repeatedly plan = construct (forever plan)

deconstruct :: Proxy a' a b' (Either x b) m -> Plan a' a b' b m x
deconstruct proxy = Plan (\k ->
  Proxy (\req res m e ->
    let aux (ReS r) = ReS (r . aux2)
        aux2 res' xorb fb' = case xorb of
          Left x -> unProxy (k x) req res' m e
          Right b -> res' b (aux . fb')
    in
      unProxy proxy req (aux2 res) m e
  ))

respond :: a -> Plan x' x a' a m a'
respond a = Plan (\k ->
  Proxy (\req res m e -> res a (\x ->
    ReS (\res' -> unProxy (k x) req res' m e))
  ))

request :: a' -> Plan a' a y' y m a
request a' = Plan (\k ->
  Proxy (\req res m e -> req a' (\x ->
    ReS (\req' -> unProxy (k x) req' res m e))
  ))

await :: Plan () a y' y m a
await = request ()

yield :: a -> Plan x' x a' a m a'
yield = respond

exit :: Plan a' a b' b m x
exit = Plan (\_ -> Proxy (\_ _ _ e -> e))


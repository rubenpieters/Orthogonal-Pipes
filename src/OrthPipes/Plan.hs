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

  , (>->)
  , each
  , filter
  , map
  , drop
  , take
  , upfrom
  , sieve
  ) where

import Prelude hiding (filter, map, drop, take)

import OrthPipes.Proxy

import Data.Void
import Data.Foldable as F


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
    let aux res' xorb fb' = case xorb of
          Left x -> unProxy (k x) req res' m e
          Right b -> res' b ((\(ReS r) -> ReS (r . aux)) . fb')
    in
      unProxy proxy req (aux res) m e
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

(>->) ::
  Plan a' a () b m r ->
  Plan () b c' c m r ->
  Plan a' a c' c m r
p1 >-> p2 = Plan (\_ -> (\() -> construct p1) +>> (construct p2))


each :: Foldable f => f a -> Plan Void () () a m ()
each = F.foldr (\a p -> yield a >> p) (return ())

filter :: (a -> Bool) -> Plan () a () a m r
filter predicate = forever $ do
  x <- await
  if predicate x
    then yield x
    else return ()

map :: (a -> b) -> Plan () a () b m r
map f = forever $ do
  x <- await
  yield (f x)

drop :: Int -> Plan () a () a m r
drop = go
  where
    go 0 = forever (await >>= yield)
    go n = do
      _ <- await
      go (n - 1)

take :: Int -> Plan () a () a m r
take n = if n == 0
  then exit
  else do
    x <- await
    yield x
    take (n - 1)

upfrom :: (Monad m) => Int -> Plan () x () Int m r
upfrom n = do
  yield n
  upfrom (n + 1)

sieve :: (Monad m) => Plan () Int () Int m r
sieve = do
  p <- await
  yield p
  filter (\x -> x `mod` p /= 0) >-> sieve

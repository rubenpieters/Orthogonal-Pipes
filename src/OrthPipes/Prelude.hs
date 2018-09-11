module OrthPipes.Prelude where

import Prelude hiding (filter, map, drop, take)

import OrthPipes.Plan

import Control.Monad

import Data.Void

unfoldr :: Monad m => (s -> m (Either r (a, s))) -> s -> Plan Void () () a m r
unfoldr step = go where
  go s0 = do
    e <- lift (step s0)
    case e of
      Left r -> return r
      Right (a, s) -> do
        yield a
        go s

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

scan :: Monad m => (x -> a -> x) -> x -> (x -> b) -> Plan () a () b m r
scan step begin done = go begin
  where
    go x = do
        yield (done x)
        a <- await
        let x' = step x a
        go $! x'

mapM :: Monad m => (a -> m b) -> Plan () a () b m r
mapM f = forever $ do
  x <- await
  b <- lift (f x)
  yield b

takeWhile :: Monad m => (a -> Bool) -> Plan () a () a m ()
takeWhile predicate = go
  where
    go = do
        a <- await
        if (predicate a)
            then do
                yield a
                go
            else return ()

dropWhile :: Monad m => (a -> Bool) -> Plan () a () a m r
dropWhile predicate = go
  where
    go = do
        a <- await
        if (predicate a)
            then go
            else do
                yield a
                cat

cat :: Monad m => Plan () a () a m r
cat = forever $ do
    x <- await
    yield x

discard :: Monad m => a -> m ()
discard _ = return ()


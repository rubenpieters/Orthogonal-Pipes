module OrthPipes.Prelude where

import Prelude hiding (filter, map, mapM, drop, take, zip, zipWith, concat)

import OrthPipes.Plan

import Control.Monad (forever, when)

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
{-# INLINABLE unfoldr #-}

filter :: (a -> Bool) -> Plan () a () a m r
filter predicate = forever $ do
  x <- await
  if predicate x
    then yield x
    else return ()
{-# INLINE[1] filter #-}

map :: (a -> b) -> Plan () a () b m r
map f = forever $ do
  x <- await
  yield (f x)
{-# INLINE[1] map #-}

drop :: Int -> Plan () a () a m r
drop = go
  where
    go 0 = forever (await >>= yield)
    go n = do
      _ <- await
      go (n - 1)
{-# INLINABLE drop #-}

take :: Int -> Plan () a () a m r
take n = if n == 0
  then exit
  else do
    x <- await
    yield x
    take (n - 1)
{-# INLINABLE take #-}

upfrom :: (Monad m) => Int -> Plan () x () Int m r
upfrom n = do
  yield n
  upfrom (n + 1)
{-# INLINABLE upfrom #-}

scan :: Monad m => (x -> a -> x) -> x -> (x -> b) -> Plan () a () b m r
scan step begin done = go begin
  where
    go x = do
        yield (done x)
        a <- await
        let x' = step x a
        go $! x'
{-# INLINABLE scan #-}

mapM :: Monad m => (a -> m b) -> Plan () a () b m r
mapM f = forever $ do
  x <- await
  b <- lift (f x)
  yield b
{-# INLINE[1] mapM #-}


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
{-# INLINABLE takeWhile #-}

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
{-# INLINABLE dropWhile #-}

discard :: Monad m => a -> m ()
discard _ = return ()

{-# INLINE[1] concat #-}
concat :: Foldable f => Plan () (f a) () a m ()
concat = forever $ do
  fa <- await
  each fa


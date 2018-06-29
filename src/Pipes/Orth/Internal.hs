{-# LANGUAGE RankNTypes #-}

module Pipes.Orth.Internal (
    ReS(..)
  , MS(..)
  , Proxy(..)
  , OrthProxy(..)
  ) where

import Control.Monad
import Control.Monad.Trans.Class (MonadTrans (lift))

newtype ReS i o a = ReS
  { unReS :: (o -> (i -> ReS i o a) -> a) -> a
  }

newtype MS m a = MS
  { unMS :: (m (MS m a) -> a) -> a
  }

newtype OrthProxy a' a b' b m r = OrthProxy
  { unOrthProxy ::
      (a' -> (a -> ReS a a' r) -> r) -> -- request
      (b -> (b' -> ReS b' b r) -> r) -> -- respond
      (m (MS m r) -> r) ->              -- lift m
      r ->                              -- exit
      r
  }

newtype Proxy a' a b' b m x = Proxy
  { unProxy :: forall r.
      (x -> OrthProxy a' a b' b m r) -> OrthProxy a' a b' b m r
  }

instance Functor (Proxy a' a b' b m) where
  fmap = liftM

instance Applicative (Proxy a' a b' b m) where
  pure = return
  (<*>) = ap

instance Monad (Proxy a' a b' b m) where
  return x = Proxy (\k -> k x)
  p >>= f = Proxy (\k -> unProxy p (\x -> unProxy (f x) k))

instance MonadTrans (Proxy a' a b' b) where
    lift mr = Proxy (\k ->
      OrthProxy (\req res m e ->
        m (fmap ((\proxy -> MS (\m' -> unOrthProxy proxy req res m' e)) . k) mr)
      ))


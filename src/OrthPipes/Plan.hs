{-# LANGUAGE RankNTypes #-}

module OrthPipes.Plan (
    Plan(..)
  , construct
  , repeatedly
  , lift

  , respond
  , request
  , await
  , yield
  , exit

  , (>->)
  , (+>>)
  , (>~)
  , each
  , cat
  , for

  , runEffect
  , fetchResponses
  , foldResponses
  ) where

import OrthPipes.Proxy

import Data.Void
import Data.Foldable as F

import Control.Monad
import Control.Monad.Trans.Class (MonadTrans (lift))

import Data.IORef
import System.IO.Unsafe

newtype Plan a' a b' b m x = Plan
  { unPlan :: forall r.
      (x -> ProxyRep a' a b' b m r) -> ProxyRep a' a b' b m r
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
  lift ma = Plan (\k req res m e -> m (fmap ((\proxy -> proxy req res m e) . k) ma))

-- construct a plan
construct :: Plan a' a b' b m x -> ProxyRep a' a b' b m r
construct (Plan plan) = plan (\_ _ _ _ e -> e)

-- construct repeated execution of a plan
repeatedly :: Plan a' a b' b m x -> ProxyRep a' a b' b m r
repeatedly plan = construct (forever plan)

-- operations

-- respond
{-# INLINE[1] respond #-}
respond :: a -> Plan x' x a' a m a'
respond a = Plan (\k req res m e ->
    unPCPar res a (PCPar (\x res' -> k x req res' m e))
  )

-- request
{-# INLINE request #-}
request :: a' -> Plan a' a y' y m a
request a' = Plan (\k req res m e ->
    unPCPar req a' (PCPar (\x req' -> k x req' res m e))
  )

-- await
{-# INLINE[1] await #-}
await :: Plan () a y' y m a
await = request ()

-- yield
{-# INLINE[1] yield #-}
yield :: a -> Plan x' x () a m ()
yield = respond

-- the exit operation aborts the pipe without returning a value
{-# INLINE exit #-}
exit :: Plan a' a b' b m x
exit = Plan (\_ _ _ _ e -> e)

-- merge plans

-- these merges have no identity element since returns are
-- not interpreted as returns

(>->) ::
  Plan a' a () b m r ->
  Plan () b c' c m r ->
  Plan a' a c' c m r
p1 >-> p2 = (\() -> p1) +>> p2
{-# INLINE[1] (>->) #-}

(+>>) ::
  (b' -> Plan a' a b' b m r) ->
  Plan b' b c' c m r ->
  Plan a' a c' c m r
p1 +>> p2 = Plan (\_ -> (construct . p1) ++>> construct p2)
{-# INLINE[1] (+>>) #-}

-- a restricted form of pipes >~
-- replace each await in q with p
(>~) ::
  Plan a' a () b m b ->
  Plan () b c' c m () ->
  Plan a' a c' c m ()
p >~ q = (\() -> go) +>> q
  where
    go = do a <- p; yield a; go
{-# INLINE[1] (>~) #-}

-- yield each element of a list
each :: Foldable f => f a -> Plan x' x () a m ()
each = F.foldr (\a p -> yield a >> p) (return ())

-- run all effects
runEffect :: (Monad m) => Plan Void () () Void m () -> m ()
runEffect x = OrthPipes.Proxy.runEffectPr (construct x)

-- fetch all output values into a list
fetchResponses :: (Monad m) => Plan x () () o m () -> m [o]
fetchResponses x = OrthPipes.Proxy.fetchResponsesPr (construct x)

-- fold over all output values
foldResponses :: (Monad m) => (b -> o -> b) -> b -> Plan x () () o m () -> m b
foldResponses combine b x = OrthPipes.Proxy.foldResponsesPr combine b (construct x)

-- yield every awaited value
cat :: Monad m => Plan () a () a m r
cat = forever $ do
    x <- await
    yield x
{-# INLINE[1] cat #-}

-- a restricted form of pipes for
-- replace each yield in p with f
for ::
  Plan x' x () b m () ->
  (b -> Plan () b c' c m ()) ->
  Plan x' x c' c m ()
for p f = (\() -> p) +>> go
  where
    go = do a <- await; f a; go
{-# INLINEABLE[0] for #-}

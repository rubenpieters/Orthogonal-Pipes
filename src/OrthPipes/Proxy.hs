{-# LANGUAGE RankNTypes #-}

module OrthPipes.Proxy (
    PCPar(..)

  , ProxyRep
  , (>++>)
  , (++>>)

  , runEffectPr
  , fetchResponsesPr
  , foldResponsesPr
  , foldProxyRep
  ) where

import Data.Void

newtype PCPar i o a = PCPar { unPCPar :: o -> PCPar o i a -> a }

type ProxyRep a' a b' b m r =
  PCPar a a' r ->         -- request
  PCPar b' b r ->         -- respond
  (m r -> r) ->           -- lift
  r ->                    -- exit
  r

-- efficient +>> implementation for ProxyRep
(++>>) ::
  (b' -> ProxyRep a' a b' b m r) ->
  ProxyRep b' b c' c m r ->
  ProxyRep a' a c' c m r
fp ++>> q = \req res m e -> q (PCPar (\b' res' -> fp b' req res' m e)) res m e

(>++>) ::
  ( b' -> ProxyRep a' a b' b m r) ->
  (_c' -> ProxyRep b' b c' c m r) ->
  (_c' -> ProxyRep a' a c' c m r)
(fb' >++> fc') c' = fb' ++>> fc' c'

-- fold ProxyRep
foldProxyRep ::
  (a' -> (a -> r) -> r) ->
  (b -> (b' -> r) -> r) ->
  (m r -> r) ->
  r ->
  ProxyRep a' a b' b m r -> r
foldProxyRep req res m e proxy = proxy (fromAlg req) (fromAlg res) m e
  where
  fromAlg :: (o -> (i -> r) -> r) -> PCPar i o r
  fromAlg alg = PCPar (\o (PCPar r) -> alg o (\i -> r i (fromAlg alg)))

-- run all effects
runEffectPr :: (Monad m) => (forall r. ProxyRep Void () () Void m r) -> m ()
runEffectPr = foldResponsesPr (\_ _ -> ()) ()

-- fetch all output values into a list
fetchResponsesPr :: (Monad m) => (forall r. ProxyRep x () () o m r) -> m [o]
fetchResponsesPr = foldResponsesPr (flip (:)) []

-- fold over all output values
foldResponsesPr :: (Monad m) => (b -> o -> b) -> b -> (forall r. ProxyRep x () () o m r) -> m b
foldResponsesPr combine b proxy = foldProxyRep
  (\_ f -> f ())
  (\o f x -> f () $! (x `combine` o))
  (\m x -> m >>= \p -> p x)
  return
  proxy
  b

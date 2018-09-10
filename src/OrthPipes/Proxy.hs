{-# LANGUAGE RankNTypes #-}

module OrthPipes.Proxy (
    PCPar(..)

  , ProxyRep
  , (>++>)
  , (++>>)

  , runEffectPr
  , fetchResponsesPr
  , foldResponsesPr
  ) where

import Data.Void

newtype PCPar i o a = PCPar { unPCPar :: o -> PCPar o i a -> a }

type ProxyRep a' a b' b m = forall r.
  PCPar a a' (m r) ->  -- request
  PCPar b' b (m r) ->  -- respond
  m r ->               -- exit
  m r

-- efficient +>> implementation for ProxyRep
(++>>) ::
  (b' -> ProxyRep a' a b' b m) ->
  ProxyRep b' b c' c m ->
  ProxyRep a' a c' c m
fp ++>> q = \req res e -> q (PCPar (\b' res' -> fp b' req res' e)) res e

(>++>) ::
  ( b' -> ProxyRep a' a b' b m) ->
  (_c' -> ProxyRep b' b c' c m) ->
  (_c' -> ProxyRep a' a c' c m)
(fb' >++> fc') c' = fb' ++>> fc' c'

-- fold ProxyRep
foldProxyRep ::
  (a' -> (a -> (m r)) -> (m r)) ->
  (b -> (b' -> (m r)) -> (m r)) ->
  m r ->
  ProxyRep a' a b' b m -> m r
foldProxyRep req res e proxy = proxy (fromAlg req) (fromAlg res) e
  where
  fromAlg :: (o -> (i -> r) -> r) -> PCPar i o r
  fromAlg alg = PCPar (\o (PCPar r) -> alg o (\i -> r i (fromAlg alg)))

-- run all effects
runEffectPr :: (Monad m) => ProxyRep Void () () Void m -> m ()
runEffectPr = foldResponsesPr (\_ _ -> ()) ()

-- fetch all output values into a list
fetchResponsesPr :: (Monad m) => ProxyRep x () () o m -> m [o]
fetchResponsesPr = foldResponsesPr (flip (:)) []

-- fold over all output values
foldResponsesPr :: (Monad m) => (b -> o -> b) -> b -> ProxyRep x () () o m -> m b
foldResponsesPr combine b proxy = foldProxyRep
  (\_ f -> f ())
  (\o f -> (`combine` o) <$> (f ()))
  (return b)
  proxy

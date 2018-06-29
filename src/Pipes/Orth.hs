module Pipes.Orth where

import Pipes.Orth.Internal
import Pipes.Orth.Core

(>->) ::
  Proxy a' a () b m r ->
  Proxy () b c' c m r ->
  Proxy a' a c' c m r
p1 >-> p2 = (\() -> p1) +>> p2


await :: Proxy () a y' y m a
await = request ()

exit :: Proxy a' a b' b m x
exit = Proxy (\_ -> OrthProxy (\_ _ _ e -> e))

module Pipes.Orth (
  -- * The Proxy Monad Transformer
    Proxy
  , X
  , Effect
  , Effect'
  , runEffect

  -- * Re-exports
  -- $reexports
  , module Control.Monad
  , module Control.Monad.IO.Class
  , module Control.Monad.Trans.Class
  , module Control.Monad.Morph
  , Foldable
  ) where

import Control.Monad (void, MonadPlus(mzero, mplus))
import Control.Monad.Catch (MonadThrow(..), MonadCatch(..))
import Control.Monad.Except (MonadError(..))
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Reader (MonadReader(..))
import Control.Monad.State (MonadState(..))
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Control.Monad.Trans.Identity (IdentityT(runIdentityT))
import Control.Monad.Trans.Maybe (MaybeT(runMaybeT))
import Control.Monad.Writer (MonadWriter(..))
import Control.Monad.Zip (MonadZip(..))

import qualified Data.Foldable as F

import Pipes.Orth.Internal
import Pipes.Orth.Core


import Control.Applicative (Alternative(..))
import Data.Semigroup

-- Re-exports
import Control.Monad.Morph (MFunctor(hoist), MMonad(embed))

(>->) ::
  Proxy a' a () b m r ->
  Proxy () b c' c m r ->
  Proxy a' a c' c m r
p1 >-> p2 = (\() -> p1) +>> p2


await :: Proxy () a y' y m a
await = request ()

exit :: Proxy a' a b' b m x
exit = Proxy (\_ -> OrthProxy (\_ _ _ e -> e))
